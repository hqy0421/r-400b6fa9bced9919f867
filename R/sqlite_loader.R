#' Resumable SQLite loading of CSV files
#'
#' Load a fixed CSV file into a SQLite table one logical record at a time with
#' crash-safe, resumable progress. Each source logical record is run through the
#' declared cleaning pipeline which may drop the record (zero result rows) or
#' expand it (several result rows). Progress is measured in *source logical
#' records*, never physical lines or target rows, so a resumed run re-parses the
#' whole file and skips the already committed record prefix.
#'
#' The parser is quote aware: newlines inside quoted fields are kept inside the
#' field, doubled quote characters (`""`) are unescaped, and a quoted `NA` or
#' quoted empty field is preserved as the literal text `"NA"` / `""` rather than
#' being treated as missing. Only an *unquoted* field matching `na` (by default
#' `NA` and the empty string) becomes `NA`.
#'
#' Only base R and the \pkg{DBI}/\pkg{RSQLite} packages (suggested, not hard
#' dependencies) are used.
#'
#' @param file Path to the fixed CSV file. The file must not change between
#'   runs: its checksum is stored and an existing progress entry is rejected if
#'   the file, the parsing options, the declared column types or the cleaning
#'   declarations have changed.
#' @param con Either an open \pkg{DBI} connection or a path to a SQLite database
#'   file (opened on demand and closed again). An in-memory database
#'   (`":memory:"`) cannot be resumed across calls because it is discarded on
#'   disconnect.
#' @param table `character(1)`. The target table name.
#' @param col_types Declared column types. A named character vector
#'   (`c(id = "integer", active = "logical")`) or, when `header = TRUE`, an
#'   unnamed vector in header order. Allowed types are `"character"`,
#'   `"integer"`, `"double"` and `"logical"`. The declaration is part of the
#'   progress fingerprint.
#' @param cleaners An optional named list of per-column cleaning functions. Each
#'   function receives `(value, i)` - the raw coerced value of the field and the
#'   source logical record index (1-based) - and returns the replacement value or
#'   `NULL` to mark the field `NA`. A `.default` entry is applied to any declared
#'   column without its own entry.
#' @param row_cleaner An optional function `(row, i)` where `row` is the one-row
#'   `data.frame` of coerced, column-cleaned values. It returns either a
#'   one-row `data.frame` of the same columns or `NULL` to filter the record out
#'   (zero output rows).
#' @param expander An optional function `(row, i)` returning a `data.frame` with
#'   the declared columns and zero, one or many rows. This is how a logical
#'   record is filtered (zero rows) or expanded into several target rows. By
#'   default every record yields its single cleaned row.
#' @param batch_size `integer(1)`. Number of *source logical records* processed
#'   per SQLite transaction/batch.
#' @param header `logical(1)`. Does the file start with a header row?
#' @param sep `character(1)`. The field separator (one byte).
#' @param quote `character(1)`. The quoting character (one byte).
#' @param na Character vector of unquoted field texts to interpret as missing.
#'   Defaults to `c("NA", "")`. Quoted fields are never converted to `NA`, even
#'   when their text matches.
#' @param encoding `character(1)`. Encoding of the file, passed to
#'   [base::rawToChar()] conversion via [base::iconv()].
#' @param restart `logical(1)`. If `TRUE`, any existing progress for this
#'   `(file, table)` is discarded and the table is rebuilt. If `FALSE` (the
#'   default), stored progress is reused and validated against the current
#'   declarations.
#' @return A `data.frame` read back from the target table, with the declared
#'   column types restored (including `logical`, stored in SQLite as integer
#'   `0`/`1`). It carries `csv_loader_meta` attributes describing the run. The
#'   returned frame is suitable for native poorman grouping, e.g.
#'   `sqlite_csv_load(...) |> poorman::group_by(...) |> poorman::summarise(...)`.
#' @export
sqlite_csv_load <- function(file, con, table,
                            col_types = character(),
                            cleaners = NULL,
                            row_cleaner = NULL,
                            expander = NULL,
                            batch_size = 1000L,
                            header = TRUE,
                            sep = ",",
                            quote = "\"",
                            na = c("NA", ""),
                            encoding = "UTF-8",
                            restart = FALSE) {
  stopifnot(
    is.character(file), length(file) == 1L, file.exists(file),
    is.character(table), length(table) == 1L, nzchar(table),
    is.numeric(batch_size), length(batch_size) == 1L, batch_size >= 1L,
    is.logical(header), length(header) == 1L, !is.na(header),
    is.character(sep), length(sep) == 1L, nchar(sep, type = "bytes") == 1L,
    is.character(quote), length(quote) == 1L, nchar(quote, type = "bytes") == 1L,
    is.character(na),
    is.character(encoding), length(encoding) == 1L,
    is.logical(restart), length(restart) == 1L, !is.na(restart)
  )
  batch_size <- as.integer(batch_size)
  parsed <- csv_parse_file(file, header = header, sep = sep, quote = quote,
                           na = na, encoding = encoding)
  file_norm <- normalizePath(file, winslash = "/", mustWork = TRUE)

  # Validate any stored progress against the new declarations before local
  # type checking, so a changed parser/type/cleaning declaration is reported as
  # a progress mismatch rather than a fresh-validation error.
  own_con <- is.character(con)
  if (own_con) con <- csv_connect(con)
  on.exit(if (own_con) DBI::dbDisconnect(con), add = TRUE)

  parse_opts <- list(header = header, sep = sep, quote = quote, na = na,
                     encoding = encoding)
  prev_progress <- csv_peek_progress(con, file_norm, table)
  if (!is.null(prev_progress) && !restart) {
    # Build the settings from the *current* call's declarations, naming the
    # columns after the stored job so the fingerprint below can detect any
    # real change (including a type change). The stored settings themselves
    # remain the reference inside csv_resume_or_init().
    stored_names <- names(prev_progress$settings$col_types)
    if (length(col_types) == 0L) {
      col_types <- prev_progress$settings$col_types
    } else {
      col_types <- csv_normalise_types(col_types, stored_names)
    }
    csv_check_cleaners(cleaners, names(col_types))
    if (!is.null(row_cleaner)) csv_check_fun(row_cleaner, "row_cleaner")
    if (!is.null(expander)) csv_check_fun(expander, "expander")
    settings <- list(
      file = file_norm, table = table, col_types = col_types,
      cleaners = cleaners, row_cleaner = row_cleaner,
      expander = expander, parse_opts = parse_opts,
      ncols = length(col_types)
    )
    settings$fingerprint <- csv_fingerprint(settings)
    meta <- csv_resume_or_init(con, settings, FALSE)
  } else {
    col_types <- csv_normalise_types(col_types, parsed$names)
    csv_check_cleaners(cleaners, names(col_types))
    if (!is.null(row_cleaner)) csv_check_fun(row_cleaner, "row_cleaner")
    if (!is.null(expander)) csv_check_fun(expander, "expander")
    settings <- list(
      file = file_norm, table = table, col_types = col_types,
      cleaners = cleaners, row_cleaner = row_cleaner,
      expander = expander, parse_opts = parse_opts,
      ncols = length(col_types)
    )
    settings$fingerprint <- csv_fingerprint(settings)
    meta <- csv_resume_or_init(con, settings, restart)
  }
  csv_ensure_target(con, table, names(col_types), col_types)
  start <- meta$committed + 1L
  n <- length(parsed$records)
  if (start > n) {
    return(csv_read_back(con, table, col_types, settings, meta, n,
                        meta$target_rows))
  }
  total_target_rows <- meta$target_rows
  while (start <= n) {
    end <- min(n, start + batch_size - 1L)
    frames <- vector("list", end - start + 1L)
    for (i in start:end) {
      frames[[i - start + 1L]] <- tryCatch(
        csv_process_record(parsed$records[[i]], i, col_types, cleaners,
                           row_cleaner, expander),
        error = function(e) {
          csv_abort(
            paste0("cleaning/expansion failed for source logical record ", i,
                   ": ", conditionMessage(e)),
            class = "csv_loader_clean_error", source_index = i,
            committed = meta$committed
          )
        }
      )
    }
    batch <- do.call(rbind, frames)
    if (!is.null(batch)) batch <- csv_prepare_for_db(batch, col_types)
    tryCatch(
      {
        DBI::dbBegin(con)
        if (!is.null(batch) && nrow(batch) > 0L) {
          DBI::dbAppendTable(con, table, batch)
        }
        DBI::dbExecute(
          con,
          paste("UPDATE", csv_qid(con, csv_progress_table()),
                "SET committed = ?, target_rows = ?, updated_at = ?",
                "WHERE fingerprint = ?"),
          params = list(end, total_target_rows +
                          if (is.null(batch)) 0L else nrow(batch),
                        csv_timestamp(), settings$fingerprint)
        )
        DBI::dbCommit(con)
      },
      error = function(e) {
        try(DBI::dbRollback(con), silent = TRUE)
        csv_abort(
          paste0("database write failed in batch of source records ",
                 start, "..", end, ": ", conditionMessage(e)),
          class = "csv_loader_db_error", source_index = start,
          committed = meta$committed
        )
      }
    )
    total_target_rows <- total_target_rows +
      if (is.null(batch)) 0L else nrow(batch)
    meta$committed <- end
    meta$target_rows <- total_target_rows
    start <- end + 1L
  }
  csv_read_back(con, table, col_types, settings, meta, n, total_target_rows)
}

#' Inspect resumable CSV loading progress
#'
#' @inheritParams sqlite_csv_load
#' @return A one-row `data.frame` of progress metadata, or `NULL` if no progress
#'   exists for `file`/`table`.
#' @export
sqlite_csv_status <- function(file, con, table) {
  own_con <- is.character(con)
  if (own_con) con <- csv_connect(con)
  on.exit(if (own_con) DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, csv_progress_table())) return(NULL)
  key <- csv_progress_key(normalizePath(file, winslash = "/", mustWork = TRUE),
                          table)
  out <- DBI::dbGetQuery(
    con,
    paste("SELECT source_file, table_name, committed, target_rows, created_at,",
          "updated_at, fingerprint FROM", csv_qid(con, csv_progress_table()),
          "WHERE progress_key = ?"),
    params = list(key)
  )
  if (nrow(out) == 0L) NULL else out
}

#' Discard resumable CSV loading progress
#'
#' Removes the progress row (and, by default, the target table) so that the next
#' [sqlite_csv_load()] call rebuilds from the first source logical record.
#'
#' @inheritParams sqlite_csv_load
#' @param drop_table `logical(1)`. Drop the target table as well as the progress
#'   row? Defaults to `TRUE`.
#' @return Invisibly, the number of progress rows removed.
#' @export
sqlite_csv_reset <- function(file, con, table, drop_table = TRUE) {
  stopifnot(is.logical(drop_table), length(drop_table) == 1L)
  own_con <- is.character(con)
  if (own_con) con <- csv_connect(con)
  on.exit(if (own_con) DBI::dbDisconnect(con), add = TRUE)
  removed <- 0L
  if (DBI::dbExistsTable(con, csv_progress_table())) {
    key <- csv_progress_key(normalizePath(file, winslash = "/", mustWork = TRUE),
                            table)
    removed <- DBI::dbExecute(
      con,
      paste("DELETE FROM", csv_qid(con, csv_progress_table()),
            "WHERE progress_key = ?"),
      params = list(key)
    )
  }
  if (drop_table && DBI::dbExistsTable(con, table)) {
    DBI::dbExecute(con, paste("DROP TABLE", csv_qid(con, table)))
  }
  invisible(removed)
}

# ---------------------------------------------------------------------------
# Quote-aware CSV parser
# ---------------------------------------------------------------------------

csv_parse_file <- function(file, header, sep, quote, na, encoding) {
  bytes <- readBin(file, what = "raw", n = file.info(file)$size)
  if (length(bytes) == 0L) {
    csv_abort("CSV file is empty", class = "csv_loader_parse_error")
  }
  # Strip a UTF-8 byte order mark if present.
  if (length(bytes) >= 3L &&
      all(bytes[1:3] == as.raw(c(0xefL, 0xbbL, 0xbfL)))) {
    bytes <- bytes[-(1:3)]
  }
  text <- rawToChar(bytes)
  if (!identical(encoding, "UTF-8")) {
    text <- iconv(text, from = encoding, to = "UTF-8")
  }
  tb <- charToRaw(text)
  sep_b <- charToRaw(sep)
  quote_b <- charToRaw(quote)
  # A final line ending terminates the last record; it does not start a new
  # one. Trim trailing break bytes (including the LF of a final CRLF).
  while (length(tb) > 0L &&
         (tb[length(tb)] == as.raw(0x0dL) || tb[length(tb)] == as.raw(0x0aL))) {
    tb <- tb[-length(tb)]
  }
  n <- length(tb)
  if (n == 0L) {
    csv_abort("CSV file contains no records", class = "csv_loader_parse_error")
  }

  is_quote <- tb == quote_b
  is_cr <- tb == as.raw(0x0dL)
  is_lf <- tb == as.raw(0x0aL)
  is_break <- is_cr | is_lf

  # The running parity of quote bytes gives the in-quote state at each byte.
  # The state *before* byte j is the parity of quotes in bytes 1..j-1.
  quote_parity <- cumsum(is_quote) %% 2L
  outside <- c(0L, quote_parity[-n]) == 0L
  if (sum(is_quote) %% 2L != 0L) {
    csv_abort(
      "unterminated quoted field in CSV file",
      class = "csv_loader_parse_error"
    )
  }
  record_breaks <- which(is_break & outside)

  # CRLF handling: keep the CR, drop the following LF so each physical line
  # ending becomes exactly one logical record break.
  if (length(record_breaks) > 1L) {
    prev <- record_breaks - 1L
    keep <- !(is_lf[record_breaks] & prev >= 1L & is_cr[prev])
    record_breaks <- record_breaks[keep]
  }

  starts <- c(1L, record_breaks + 1L)
  ends <- c(record_breaks - 1L, n)
  if (length(starts) == 0L) {
    csv_abort("CSV file contains no records", class = "csv_loader_parse_error")
  }

  parse_one <- function(rs, re) {
    if (rs > re) return(list(text = character(), quoted = logical()))
    chunk <- tb[rs:re]
    q <- chunk == quote_b
    s <- chunk == sep_b
    parity <- cumsum(q) %% 2L
    # State *before* each byte: parity of the quote bytes preceding it.
    fstate <- c(0L, parity[-length(parity)]) == 0L
    fbreaks <- which(s & fstate)
    if (sum(q) %% 2L != 0L) {
      csv_abort("unterminated quoted field in CSV record",
                class = "csv_loader_parse_error")
    }
    fstarts <- c(1L, fbreaks + 1L)
    fends <- c(fbreaks - 1L, length(chunk))
    fends <- pmax(fends, fstarts - 1L)
    texts <- character(length(fstarts))
    quoted <- logical(length(fstarts))
    for (j in seq_along(fstarts)) {
      a <- fstarts[j]
      b <- fends[j]
      if (b < a) {
        texts[j] <- ""
        next
      }
      field <- chunk[a:b]
      is_quoted <- length(field) >= 2L && field[1L] == quote_b &&
        field[length(field)] == quote_b
      quoted[j] <- is_quoted
      if (is_quoted) {
        field <- field[-c(1L, length(field))]
        # Unescape doubled quote characters ("" -> ").
        if (length(field) >= 2L) {
          qpos <- which(field == quote_b)
          if (length(qpos) >= 2L) {
            drop <- qpos[qpos %in% (qpos - 1L)]
            if (length(drop) > 0L) field <- field[-drop]
          }
        }
      }
      texts[j] <- rawToChar(field)
    }
    list(text = texts, quoted = quoted)
  }

  rows <- lapply(seq_along(starts), function(k) parse_one(starts[k], ends[k]))
  field_counts <- vapply(rows, function(r) length(r$text), integer(1L))
  nfields <- unique(field_counts)
  if (length(nfields) != 1L) {
    csv_abort(
      paste0("ragged CSV records: found ",
             paste(nfields, collapse = "/"), " fields per record"),
      class = "csv_loader_parse_error"
    )
  }

  header_names <- NULL
  first <- 1L
  if (header) {
    header_names <- make.names(rows[[1L]]$text, unique = TRUE)
    first <- 2L
  }
  if (first > length(rows)) {
    return(list(names = header_names %||% paste0("V", seq_len(nfields)),
                records = list(), ncols = nfields))
  }

  records <- lapply(rows[first:length(rows)], function(r) {
    vals <- r$text
    miss <- !r$quoted & vals %in% na
    vals[miss] <- NA_character_
    vals
  })
  list(
    names = header_names %||% paste0("V", seq_len(nfields)),
    records = records,
    ncols = nfields
  )
}

# ---------------------------------------------------------------------------
# Declared types and cleaning
# ---------------------------------------------------------------------------

csv_allowed_types <- c("character", "integer", "double", "logical")

csv_normalise_types <- function(col_types, header_names) {
  if (length(col_types) == 0L) {
    col_types <- setNames(rep("character", length(header_names)), header_names)
  }
  if (is.list(col_types)) col_types <- unlist(col_types, use.names = TRUE)
  stopifnot(is.character(col_types))
  if (!is.null(names(col_types)) && anyDuplicated(names(col_types)) > 0L) {
    csv_abort(
      paste0("col_types has duplicated column name(s): ",
             paste(unique(names(col_types)[duplicated(names(col_types))]),
                   collapse = ", ")),
      class = "csv_loader_type_error"
    )
  }
  bad <- setdiff(col_types, csv_allowed_types)
  if (length(bad) > 0L) {
    csv_abort(
      paste0("unsupported declared type(s): ", paste(unique(bad), collapse = ", "),
             ". Allowed: ", paste(csv_allowed_types, collapse = ", ")),
      class = "csv_loader_type_error"
    )
  }
  if (is.null(names(col_types))) {
    if (length(col_types) != length(header_names)) {
      csv_abort(
        paste0("length(col_types) = ", length(col_types),
               " but the CSV has ", length(header_names), " columns"),
        class = "csv_loader_type_error"
      )
    }
    names(col_types) <- header_names
  } else {
    missing_cols <- setdiff(header_names, names(col_types))
    if (length(missing_cols) > 0L) {
      csv_abort(
        paste0("col_types has no declaration for column(s): ",
               paste(missing_cols, collapse = ", ")),
        class = "csv_loader_type_error"
      )
    }
    extra <- setdiff(names(col_types), header_names)
    if (length(extra) > 0L) {
      csv_abort(
        paste0("col_types declares unknown column(s): ",
               paste(extra, collapse = ", ")),
        class = "csv_loader_type_error"
      )
    }
    col_types <- col_types[header_names]
  }
  setNames(as.character(col_types), names(col_types))
}

csv_coerce <- function(x, type, column, i) {
  if (type == "character") return(as.character(x))
  if (all(is.na(x))) {
    return(switch(type, integer = NA_integer_, double = NA_real_,
                  logical = NA))
  }
  if (type == "integer") {
    if (all(grepl("^[+-]?[0-9]+$", x[!is.na(x)]), na.rm = TRUE)) {
      vals <- suppressWarnings(as.integer(x))
      if (any(is.na(vals) != is.na(x))) {
        csv_abort(
          paste0("integer out of range in column '", column,
                 "' at record ", i),
          class = "csv_loader_type_error", source_index = i
        )
      }
      return(vals)
    }
    vals <- suppressWarnings(as.numeric(x))
    if (any(is.na(vals) != is.na(x)) || any(vals[!is.na(vals)] !=
                                            floor(vals[!is.na(vals)]))) {
      csv_abort(
        paste0("cannot coerce column '", column, "' to integer at record ", i),
        class = "csv_loader_type_error", source_index = i
      )
    }
    return(as.integer(vals))
  }
  if (type == "double") {
    vals <- suppressWarnings(as.numeric(x))
    if (any(is.na(vals) != is.na(x))) {
      csv_abort(
        paste0("cannot coerce column '", column, "' to double at record ", i),
        class = "csv_loader_type_error", source_index = i
      )
    }
    return(vals)
  }
  if (type == "logical") {
    lut <- c("true" = TRUE, "false" = FALSE, "t" = TRUE, "f" = FALSE,
             "1" = TRUE, "0" = FALSE)
    key <- tolower(x[!is.na(x)])
    if (any(!key %in% names(lut))) {
      csv_abort(
        paste0("cannot coerce column '", column,
               "' to logical at record ", i, " (0/FALSE are valid)"),
        class = "csv_loader_type_error", source_index = i
      )
    }
    out <- rep(NA, length(x))
    out[!is.na(x)] <- lut[key]
    return(out)
  }
}

csv_process_record <- function(raw, i, col_types, cleaners,
                               row_cleaner, expander) {
  cols <- names(col_types)
  row <- as.data.frame(
    setNames(lapply(cols, function(nm) {
      csv_coerce(raw[[match(nm, cols)]], col_types[[nm]], nm, i)
    }), cols),
    stringsAsFactors = FALSE, optional = TRUE
  )
  if (!is.null(cleaners)) {
    default <- cleaners[[".default"]]
    for (nm in cols) {
      f <- cleaners[[nm]]
      if (is.null(f)) f <- default
      if (!is.null(f)) {
        v <- f(row[[nm]][1L], i)
        row[[nm]][1L] <- if (is.null(v)) NA else v
      }
    }
  }
  if (!is.null(row_cleaner)) {
    row <- row_cleaner(row, i)
    if (is.null(row)) return(NULL)
    csv_check_row_shape(row, cols, i)
  }
  if (is.null(expander)) return(row)
  out <- expander(row, i)
  if (is.null(out) || NROW(out) == 0L) return(NULL)
  csv_check_row_shape(out, cols, i, allow_many = TRUE)
  out
}

csv_check_row_shape <- function(x, cols, i, allow_many = FALSE) {
  if (!is.data.frame(x)) {
    csv_abort(paste0("pipeline result at record ", i, " is not a data.frame"),
              class = "csv_loader_clean_error", source_index = i)
  }
  if (!identical(names(x), cols)) {
    csv_abort(
      paste0("pipeline result at record ", i,
             " must keep the declared columns in order"),
      class = "csv_loader_clean_error", source_index = i
    )
  }
  if (!allow_many && nrow(x) != 1L) {
    csv_abort(paste0("row_cleaner at record ", i,
                     " must return one row or NULL"),
              class = "csv_loader_clean_error", source_index = i)
  }
}

csv_check_cleaners <- function(cleaners, cols) {
  if (is.null(cleaners)) return(invisible())
  if (!is.list(cleaners) || is.null(names(cleaners)) ||
      any(!nzchar(names(cleaners)))) {
    csv_abort("cleaners must be a named list of functions",
              class = "csv_loader_decl_error")
  }
  unknown <- setdiff(names(cleaners), c(cols, ".default"))
  if (length(unknown) > 0L) {
    csv_abort(paste0("cleaners reference unknown column(s): ",
                     paste(unknown, collapse = ", ")),
              class = "csv_loader_decl_error")
  }
  for (nm in names(cleaners)) csv_check_fun(cleaners[[nm]],
                                            paste0("cleaners[[", nm, "]]"))
}

csv_check_fun <- function(f, what) {
  if (!is.function(f)) {
    csv_abort(paste0(what, " must be a function"),
              class = "csv_loader_decl_error")
  }
}

# ---------------------------------------------------------------------------
# SQLite bookkeeping
# ---------------------------------------------------------------------------

csv_progress_table <- function() "csv_loader_progress"

csv_sql_type <- function(r_type) {
  switch(r_type, character = "TEXT", integer = "INTEGER",
         double = "REAL", logical = "INTEGER", "TEXT")
}

csv_connect <- function(path) {
  if (!requireNamespace("RSQLite", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) {
    csv_abort(
      "the DBI and RSQLite packages are required for SQLite loading",
      class = "csv_loader_dependency_error"
    )
  }
  DBI::dbConnect(RSQLite::SQLite(), path)
}

csv_qid <- function(con, x) DBI::dbQuoteIdentifier(con, x)

csv_progress_key <- function(file, table) paste0(file, "||", table)

csv_fingerprint <- function(settings) {
  payload <- serialize(settings[c("file", "table", "col_types", "cleaners",
                                  "row_cleaner", "expander", "parse_opts",
                                  "ncols")], NULL)
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(payload, tmp)
  paste(tools::md5sum(tmp), collapse = "")
}

csv_file_hash <- function(file) {
  paste(tools::md5sum(file), collapse = "")
}

csv_timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

csv_ensure_progress <- function(con) {
  if (DBI::dbExistsTable(con, csv_progress_table())) return(invisible())
  DBI::dbExecute(
    con,
    paste("CREATE TABLE", csv_qid(con, csv_progress_table()), "(",
          "progress_key TEXT PRIMARY KEY,",
          "source_file TEXT NOT NULL,",
          "table_name TEXT NOT NULL,",
          "committed INTEGER NOT NULL,",
          "target_rows INTEGER NOT NULL,",
          "file_hash TEXT NOT NULL,",
          "settings BLOB NOT NULL,",
          "fingerprint TEXT NOT NULL,",
          "created_at TEXT NOT NULL,",
          "updated_at TEXT NOT NULL)")
  )
}

csv_ensure_target <- function(con, table, cols, col_types) {
  if (DBI::dbExistsTable(con, table)) {
    actual <- DBI::dbGetQuery(con, paste("PRAGMA table_info(",
                                         csv_qid(con, table), ")"))
    if (!identical(actual$name, cols)) {
      csv_abort(
        paste0("existing table '", table,
               "' does not match the declared columns"),
        class = "csv_loader_decl_error"
      )
    }
    return(invisible())
  }
  defs <- paste(csv_qid(con, cols), vapply(col_types, csv_sql_type,
                                           character(1L)), collapse = ", ")
  DBI::dbExecute(con, paste("CREATE TABLE", csv_qid(con, table), "(", defs, ")"))
}

csv_peek_progress <- function(con, file, table) {
  if (!DBI::dbExistsTable(con, csv_progress_table())) return(NULL)
  key <- csv_progress_key(file, table)
  prev <- DBI::dbGetQuery(
    con,
    paste("SELECT * FROM", csv_qid(con, csv_progress_table()),
          "WHERE progress_key = ?"),
    params = list(key)
  )
  if (nrow(prev) == 0L) return(NULL)
  list(row = prev, settings = csv_unserialize(prev$settings[[1L]]))
}

csv_resume_or_init <- function(con, settings, restart) {
  csv_ensure_progress(con)
  key <- csv_progress_key(settings$file, settings$table)
  prev <- DBI::dbGetQuery(
    con,
    paste("SELECT * FROM", csv_qid(con, csv_progress_table()),
          "WHERE progress_key = ?"),
    params = list(key)
  )
  if (nrow(prev) == 0L) {
    if (restart) {
      if (DBI::dbExistsTable(con, settings$table)) {
        DBI::dbExecute(con, paste("DROP TABLE",
                                  csv_qid(con, settings$table)))
      }
    }
    DBI::dbExecute(
      con,
      paste("INSERT INTO", csv_qid(con, csv_progress_table()),
            "(progress_key, source_file, table_name, committed, target_rows,",
            "file_hash, settings, fingerprint, created_at, updated_at)",
            "VALUES (?, ?, ?, 0, 0, ?, ?, ?, ?, ?)"),
      params = list(
        key, settings$file, settings$table, csv_file_hash(settings$file),
        csv_serialize(settings), settings$fingerprint,
        csv_timestamp(), csv_timestamp()
      )
    )
    return(list(committed = 0L, target_rows = 0L))
  }
  if (restart) {
    DBI::dbExecute(
      con,
      paste("DELETE FROM", csv_qid(con, csv_progress_table()),
            "WHERE progress_key = ?"),
      params = list(key)
    )
    if (DBI::dbExistsTable(con, settings$table)) {
      DBI::dbExecute(con, paste("DROP TABLE", csv_qid(con, settings$table)))
    }
    return(csv_resume_or_init(con, settings, FALSE))
  }
  old <- csv_unserialize(prev$settings[[1L]])
  diffs <- csv_settings_diff(old, settings)
  current_hash <- csv_file_hash(settings$file)
  if (!identical(prev$file_hash, current_hash)) {
    diffs <- c(diffs, "source file contents (checksum mismatch)")
  }
  if (length(diffs) > 0L) {
    csv_abort(
      paste0("stored progress for '", settings$table,
             "' is incompatible with the current job: ",
             paste(diffs, collapse = "; "),
             ". Use restart = TRUE or sqlite_csv_reset() to start over."),
      class = "csv_loader_progress_mismatch"
    )
  }
  list(committed = as.integer(prev$committed),
       target_rows = as.integer(prev$target_rows))
}

csv_settings_diff <- function(old, new) {
  diffs <- character()
  if (!identical(old$parse_opts, new$parse_opts)) {
    diffs <- c(diffs, "parsing options (header/sep/quote/na/encoding)")
  }
  if (!identical(old$col_types, new$col_types)) {
    diffs <- c(diffs, "declared column types")
  }
  if (!identical(names(old$cleaners), names(new$cleaners)) ||
      !identical(deparse(old$cleaners), deparse(new$cleaners))) {
    diffs <- c(diffs, "cleaning declarations (cleaners)")
  }
  if (!identical(deparse(old$row_cleaner), deparse(new$row_cleaner))) {
    diffs <- c(diffs, "cleaning declarations (row_cleaner)")
  }
  if (!identical(deparse(old$expander), deparse(new$expander))) {
    diffs <- c(diffs, "cleaning declarations (expander)")
  }
  diffs
}

csv_prepare_for_db <- function(batch, col_types) {
  for (nm in names(col_types)) {
    if (col_types[[nm]] == "logical" && !is.integer(batch[[nm]])) {
      batch[[nm]] <- as.integer(batch[[nm]])
    }
    if (col_types[[nm]] == "character") {
      batch[[nm]] <- as.character(batch[[nm]])
    }
  }
  batch
}

csv_read_back <- function(con, table, col_types, settings, meta, n_source,
                          target_rows) {
  out <- DBI::dbGetQuery(
    con,
    paste("SELECT * FROM", csv_qid(con, table), "ORDER BY rowid")
  )
  for (nm in names(col_types)) {
    if (col_types[[nm]] == "logical" && nm %in% names(out)) {
      out[[nm]] <- as.logical(out[[nm]])
    }
    if (col_types[[nm]] == "character" && nm %in% names(out)) {
      out[[nm]] <- as.character(out[[nm]])
    }
  }
  attr(out, "csv_loader_meta") <- list(
    source_file = settings$file,
    table = table,
    source_records = n_source,
    committed = meta$committed,
    target_rows = target_rows,
    fingerprint = settings$fingerprint,
    col_types = col_types
  )
  out
}

csv_serialize <- function(x) {
  rawToChar(serialize(x, NULL, ascii = TRUE))
}

csv_unserialize <- function(x) {
  if (is.raw(x)) x <- rawToChar(x)
  unserialize(charToRaw(x))
}

csv_abort <- function(message, class = NULL, source_index = NA_integer_,
                      committed = NA_integer_) {
  cond <- structure(
    class = c(class, "csv_loader_error", "error", "condition"),
    list(message = message, source_index = source_index,
         committed = committed)
  )
  stop(cond)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
