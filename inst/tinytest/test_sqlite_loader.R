# Tests for the resumable SQLite CSV loader. Skipped unless DBI/RSQLite exist.
if (!requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("RSQLite", quietly = TRUE)) {
  exit_file("DBI/RSQLite not available")
}


tl_wd <- tempfile("csvloader_"); dir.create(tl_wd)
tl_csv <- file.path(tl_wd, "data.csv")
tl_db <- file.path(tl_wd, "data.db")

tl_write <- function(lines, path = tl_csv) writeLines(lines, path, useBytes = TRUE)
tl_types <- c(id = "integer", grp = "character", val = "double",
           flag = "logical", note = "character")

# ---------------------------------------------------------------------------
# Parser: quoted newlines, NA vs empty text, doubled quotes, CRLF
# ---------------------------------------------------------------------------
tl_write(c(
  "id,grp,val,flag,note",
  '1,a,1.5,FALSE,"line one
line two"',
  '2,b,,0,"say ""hi"""',
  "3,c,3.0,TRUE,NA",
  '4,d,4.25,1,""'
))
tl_parsed <- poorman:::csv_parse_file(tl_csv, TRUE, ",", "\"", c("NA", ""), "UTF-8")
expect_equal(length(tl_parsed$records), 4L, info = "quoted newline is one logical record")
expect_equal(tl_parsed$names, c("id", "grp", "val", "flag", "note"))
expect_true(grepl("\n", tl_parsed$records[[1L]][5L]), info = "newline preserved inside quotes")
expect_equal(tl_parsed$records[[2L]][5L], 'say "hi"', info = "doubled quotes unescaped")
expect_true(is.na(tl_parsed$records[[2L]][3L]), info = "unquoted empty -> NA")
expect_equal(tl_parsed$records[[4L]][5L], "", info = "quoted empty stays empty text")
expect_true(is.na(tl_parsed$records[[3L]][5L]), info = "unquoted NA -> NA")
expect_equal(tl_parsed$records[[2L]][4L], "0", info = "zero kept as text before coercion")

# Quoted NA text is not missing.
tl_write(c("x,y", '1,"NA"', "NA,2"))
tl_parsed2 <- poorman:::csv_parse_file(tl_csv, TRUE, ",", "\"", "NA", "UTF-8")
expect_equal(tl_parsed2$records[[1L]][2L], "NA", info = "quoted NA preserved as text")
expect_true(is.na(tl_parsed2$records[[2L]][1L]), info = "unquoted NA is missing")

# CRLF line endings and a quoted CRLF.
writeBin(charToRaw(paste0(
  "a,b\r\n",
  '1,"x\r\ny"\r\n',
  "2,3\r\n")), tl_csv)
tl_parsed3 <- poorman:::csv_parse_file(tl_csv, TRUE, ",", "\"", c("NA", ""), "UTF-8")
expect_equal(length(tl_parsed3$records), 2L, info = "CRLF record breaks")
expect_true(grepl("\r\n", tl_parsed3$records[[1L]][2L]),
            info = "CRLF kept inside quoted field")

# Unterminated quote and ragged rows are rejected.
tl_write(c("a,b", '1,"oops'))
expect_error(poorman:::csv_parse_file(tl_csv, TRUE, ",", "\"", "NA", "UTF-8"),
             pattern = "unterminated")
tl_write(c("a,b", "1,2,3", "4,5"))
expect_error(poorman:::csv_parse_file(tl_csv, TRUE, ",", "\"", "NA", "UTF-8"),
             pattern = "ragged")

# Headerless file gets V names; custom separator.
tl_write(c("1|x", "2|y"))
tl_parsed4 <- poorman:::csv_parse_file(tl_csv, FALSE, "|", "\"", "NA", "UTF-8")
expect_equal(tl_parsed4$names, c("V1", "V2"), info = "synthetic header names")
expect_equal(length(tl_parsed4$records), 2L)

# ---------------------------------------------------------------------------
# Declared types: 0/FALSE, coercion failures, integer/double/logical
# ---------------------------------------------------------------------------
tl_write(c("i,d,l,t", "1,1.5,FALSE,a", "0,2,0,b", "3,,TRUE,c", "4,4.5,1,d"))
out <- poorman:::sqlite_csv_load(tl_csv, tl_db, "typed",
            col_types = c(i = "integer", d = "double", l = "logical",
                          t = "character"),
            batch_size = 2L)
expect_equal(class(out$i), "integer", info = "integer column type")
expect_equal(out$i, c(1L, 0L, 3L, 4L), info = "zero integer preserved")
expect_true(is.double(out$d), info = "double column type")
expect_true(is.na(out$d[3L]), info = "missing double")
expect_equal(class(out$l), "logical", info = "logical restored on read back")
expect_equal(out$l, c(FALSE, FALSE, TRUE, TRUE), info = "0/FALSE -> FALSE")
expect_equal(class(out$t), "character")

# Type affinity is really declared in SQLite.
tl_con <- DBI::dbConnect(RSQLite::SQLite(), tl_db)
info <- DBI::dbGetQuery(tl_con, "PRAGMA table_info(typed)")
DBI::dbDisconnect(tl_con)
expect_equal(info$type, c("INTEGER", "REAL", "INTEGER", "TEXT"),
             info = "SQLite storage types follow declarations")

# Bad logical / integer text fails without advancing progress.
tl_write(c("i,l", "1,TRUE", "2,maybe"))
poorman:::sqlite_csv_reset(tl_csv, tl_db, "bad")
expect_error(poorman:::sqlite_csv_load(tl_csv, tl_db, "bad",
                  col_types = c(i = "integer", l = "logical"),
                  batch_size = 5L),
             pattern = "logical")
expect_equal(poorman:::sqlite_csv_status(tl_csv, tl_db, "bad")$committed, 0L,
             info = "coercion failure does not advance the prefix")

# ---------------------------------------------------------------------------
# Filtering to zero still advances; expansion produces many rows
# ---------------------------------------------------------------------------
tl_write(c("id,grp,val,flag,note",
            "1,a,10,FALSE,x",
            "2,b,20,TRUE,y",
            "3,a,30,FALSE,z",
            "4,c,40,TRUE,w"))
# expander: drop grp == "c" entirely; duplicate grp == "a" records into 2 rows.
tl_expander <- function(row, i) {
  if (row$grp == "c") return(NULL)
  if (row$grp == "a") return(rbind(row, row))
  row
}
poorman:::sqlite_csv_reset(tl_csv, tl_db, "exp")
out <- poorman:::sqlite_csv_load(tl_csv, tl_db, "exp", col_types = tl_types, expander = tl_expander,
            batch_size = 2L)
expect_equal(nrow(out), 5L, info = "1+2+0+1 expansion/filter target rows")
expect_equal(out$id, c(1L, 1L, 2L, 3L, 3L), info = "expanded rows in source order")
expect_false(4L %in% out$id, info = "filtered record absent")
prog <- poorman:::sqlite_csv_status(tl_csv, tl_db, "exp")
expect_equal(prog$committed, 4L, info = "all 4 source records committed")
expect_equal(prog$target_rows, 5L,
             info = "progress counts source records, not target rows")

# row_cleaner filtering to zero also advances.
poorman:::sqlite_csv_reset(tl_csv, tl_db, "rc")
out <- poorman:::sqlite_csv_load(tl_csv, tl_db, "rc", col_types = tl_types,
            row_cleaner = function(row, i) if (row$flag) row else NULL,
            batch_size = 1L)
expect_equal(out$id, c(2L, 4L))
expect_equal(poorman:::sqlite_csv_status(tl_csv, tl_db, "rc")$committed, 4L,
             info = "filtered-to-zero records still advance the prefix")

# Per-column cleaners and .default; NULL return means NA.
poorman:::sqlite_csv_reset(tl_csv, tl_db, "cl")
out <- poorman:::sqlite_csv_load(tl_csv, tl_db, "cl", col_types = tl_types,
            cleaners = list(
              grp = function(v, i) toupper(v),
              val = function(v, i) NULL,
              .default = function(v, i) v
            ), batch_size = 3L)
expect_equal(out$grp, c("A", "B", "A", "C"), info = "column cleaner applied")
expect_true(all(is.na(out$val)), info = "cleaner NULL -> NA")

# Cleaner errors stop at the offending source record; earlier batches persist.
poorman:::sqlite_csv_reset(tl_csv, tl_db, "ce")
err <- tryCatch(
  poorman:::sqlite_csv_load(tl_csv, tl_db, "ce", col_types = tl_types, batch_size = 2L,
       cleaners = list(grp = function(v, i) {
         if (i == 3L) stop("boom at 3") else v
       })),
  error = function(e) e
)
expect_inherits(err, "csv_loader_clean_error")
expect_equal(err$source_index, 3L, info = "error names the source record")
expect_equal(err$committed, 2L, info = "only the completed prefix survives")
expect_equal(poorman:::sqlite_csv_status(tl_csv, tl_db, "ce")$committed, 2L,
             info = "failed record not skipped on resume")
# Resuming with the same broken cleaner fails again at record 3.
err2 <- tryCatch(
  poorman:::sqlite_csv_load(tl_csv, tl_db, "ce", col_types = tl_types, batch_size = 2L,
       cleaners = list(grp = function(v, i) {
         if (i == 3L) stop("boom at 3") else v
       })),
  error = function(e) e
)
expect_equal(err2$source_index, 3L, info = "resume re-processes record 3")
# A changed cleaning declaration rejects the old progress (mandated contract).
err3 <- tryCatch(
  poorman:::sqlite_csv_load(tl_csv, tl_db, "ce", col_types = tl_types,
            cleaners = list(grp = function(v, i) v), batch_size = 2L),
  error = function(e) e
)
expect_inherits(err3, "csv_loader_progress_mismatch")
# Restart with the repaired cleaner rebuilds the whole file, no duplicates.
out <- poorman:::sqlite_csv_load(tl_csv, tl_db, "ce", col_types = tl_types,
            cleaners = list(grp = function(v, i) v), batch_size = 2L,
            restart = TRUE)
expect_equal(out$id, 1:4, info = "restart after cleaner repair rebuilds")
expect_equal(poorman:::sqlite_csv_status(tl_csv, tl_db, "ce")$committed, 4L)

# ---------------------------------------------------------------------------
# Resume skips the committed source prefix by re-parsing the whole file
# ---------------------------------------------------------------------------
poorman:::sqlite_csv_reset(tl_csv, tl_db, "rs")
o1 <- poorman:::sqlite_csv_load(tl_csv, tl_db, "rs", col_types = tl_types, batch_size = 2L)
o2 <- poorman:::sqlite_csv_load(tl_csv, tl_db, "rs", col_types = tl_types, batch_size = 3L)
expect_equal(o1, o2, info = "reopening reads identical data")
expect_equal(nrow(o2), 4L)
expect_equal(poorman:::sqlite_csv_status(tl_csv, tl_db, "rs")$committed, 4L)

# Progress is keyed on logical records, not physical lines: a file where every
# record spans two physical lines still resumes by record count.
tl_write(c(
  "id,grp,val,flag,note",
  '1,a,1,FALSE,"l1
l2"',
  '2,b,2,TRUE,"l3
l4"'
))
poorman:::sqlite_csv_reset(tl_csv, tl_db, "nl")
poorman:::sqlite_csv_load(tl_csv, tl_db, "nl", col_types = tl_types, batch_size = 1L)
expect_equal(poorman:::sqlite_csv_status(tl_csv, tl_db, "nl")$committed, 2L,
             info = "two logical records despite four physical lines")

# ---------------------------------------------------------------------------
# Transaction rollback: database failure leaves the prefix untouched
# ---------------------------------------------------------------------------
tl_write(c("id,grp,val,flag,note",
            "1,a,10,FALSE,x", "2,b,20,TRUE,y",
            "3,a,30,FALSE,z", "4,c,40,TRUE,w"))
poorman:::sqlite_csv_reset(tl_csv, tl_db, "tx")
poorman:::sqlite_csv_load(tl_csv, tl_db, "tx", col_types = tl_types, batch_size = 2L)
# Simulate a process crash after the first batch: only the first 2 source
# records are marked committed, although all 4 target rows are physically
# present. Drop the uncommitted rows so the resume genuinely re-inserts them,
# then install a trigger that fails that re-insertion.
tl_con <- DBI::dbConnect(RSQLite::SQLite(), tl_db)
DBI::dbExecute(tl_con, "DELETE FROM tx WHERE id > 2")
DBI::dbExecute(tl_con, paste(
  "UPDATE csv_loader_progress SET committed = 2, target_rows = 2",
  "WHERE table_name = 'tx'"))
DBI::dbExecute(tl_con, paste(
  "CREATE TRIGGER tx_fail BEFORE INSERT ON tx",
  "BEGIN",
  "SELECT CASE WHEN NEW.id > 2 THEN RAISE(ABORT, 'injected write failure') END;",
  "END;"))
DBI::dbDisconnect(tl_con)
err <- tryCatch(poorman:::sqlite_csv_load(tl_csv, tl_db, "tx", col_types = tl_types,
                     batch_size = 2L), error = function(e) e)
expect_inherits(err, "csv_loader_db_error")
expect_equal(poorman:::sqlite_csv_status(tl_csv, tl_db, "tx")$committed, 2L,
             info = "failed batch rolled back, prefix retained")
# Drop the trigger and resume from record 3.
tl_con <- DBI::dbConnect(RSQLite::SQLite(), tl_db)
DBI::dbExecute(tl_con, "DROP TRIGGER tx_fail")
DBI::dbDisconnect(tl_con)
out <- poorman:::sqlite_csv_load(tl_csv, tl_db, "tx", col_types = tl_types, batch_size = 2L)
expect_equal(out$id, 1:4, info = "resume after rolled-back batch")

# ---------------------------------------------------------------------------
# Fingerprint: file / parse options / types / cleaning declarations
# ---------------------------------------------------------------------------
poorman:::sqlite_csv_reset(tl_csv, tl_db, "fp")
poorman:::sqlite_csv_load(tl_csv, tl_db, "fp", col_types = tl_types, batch_size = 2L)
# Changed file bytes.
b <- readBin(tl_csv, "raw", file.info(tl_csv)$size)
writeBin(c(b, charToRaw("5,d,50,FALSE,q\n")), tl_csv)
expect_error(poorman:::sqlite_csv_load(tl_csv, tl_db, "fp", col_types = tl_types),
             pattern = "checksum mismatch")
# Remove the appended line again, then change parsing options.
writeBin(b, tl_csv)
expect_error(poorman:::sqlite_csv_load(tl_csv, tl_db, "fp", col_types = tl_types, sep = ";"),
             pattern = "parsing options")
expect_error(poorman:::sqlite_csv_load(tl_csv, tl_db, "fp", col_types = tl_types, na = "N/A"),
             pattern = "parsing options")
# Changed declared type.
ch <- tl_types; ch[["id"]] <- "double"
expect_error(poorman:::sqlite_csv_load(tl_csv, tl_db, "fp", col_types = ch),
             pattern = "declared column types")
# Changed cleaning declarations (same col_types).
expect_error(
  poorman:::sqlite_csv_load(tl_csv, tl_db, "fp", col_types = tl_types,
       cleaners = list(grp = function(v, i) toupper(v))),
  pattern = "cleaning declarations"
)
expect_error(
  poorman:::sqlite_csv_load(tl_csv, tl_db, "fp", col_types = tl_types,
       expander = function(row, i) row),
  pattern = "cleaning declarations"
)
# A restart ignores the old progress and rebuilds cleanly.
out <- poorman:::sqlite_csv_load(tl_csv, tl_db, "fp", col_types = tl_types, restart = TRUE)
expect_equal(nrow(out), 4L)

# ---------------------------------------------------------------------------
# Read-back data works with native poorman grouping
# ---------------------------------------------------------------------------
tl_write(c("id,grp,val,flag,note",
            "1,a,10,FALSE,x", "2,b,20,TRUE,y",
            "3,a,30,FALSE,z", "4,c,40,TRUE,w"))
poorman:::sqlite_csv_reset(tl_csv, tl_db, "grp")
loaded <- poorman:::sqlite_csv_load(tl_csv, tl_db, "grp", col_types = tl_types, batch_size = 2L)
agg <- loaded |>
  poorman::group_by(grp) |>
  poorman::summarise(n = poorman::n(), total = sum(val),
                     any_flag = any(flag)) |>
  poorman::arrange(grp)
expect_equal(agg$grp, c("a", "b", "c"))
expect_equal(agg$n, c(2L, 1L, 1L))
expect_equal(agg$total, c(40, 20, 40))
expect_equal(agg$any_flag, c(FALSE, TRUE, TRUE),
             info = "logical type survives SQLite round trip for grouping")

# ---------------------------------------------------------------------------
# Connection handling and reset behaviour
# ---------------------------------------------------------------------------
# Passing an open connection works and is left open.
tl_con <- DBI::dbConnect(RSQLite::SQLite(), tl_db)
poorman:::sqlite_csv_reset(tl_csv, tl_con, "opencon")
out <- poorman:::sqlite_csv_load(tl_csv, tl_con, "opencon", col_types = tl_types)
expect_true(DBI::dbIsValid(tl_con), info = "user-owned connection stays open")
expect_equal(nrow(out), 4L)
DBI::dbDisconnect(tl_con)

# reset removes progress; drop_table = FALSE keeps the table.
poorman:::sqlite_csv_reset(tl_csv, tl_db, "keep")
poorman:::sqlite_csv_load(tl_csv, tl_db, "keep", col_types = tl_types)
poorman:::sqlite_csv_reset(tl_csv, tl_db, "keep", drop_table = FALSE)
expect_null(poorman:::sqlite_csv_status(tl_csv, tl_db, "keep"))
tl_con <- DBI::dbConnect(RSQLite::SQLite(), tl_db)
expect_true(DBI::dbExistsTable(tl_con, "keep"), info = "table kept on request")
DBI::dbDisconnect(tl_con)

# Empty result table still reads back as an empty, typed data.frame.
tl_write(c("id,grp,val,flag,note", "1,a,1,FALSE,x"))
poorman:::sqlite_csv_reset(tl_csv, tl_db, "empty")
out <- poorman:::sqlite_csv_load(tl_csv, tl_db, "empty", col_types = tl_types,
            expander = function(row, i) NULL)
expect_equal(nrow(out), 0L)
expect_equal(names(out), names(tl_types))
expect_equal(poorman:::sqlite_csv_status(tl_csv, tl_db, "empty")$committed, 1L,
             info = "all-filtered file still reaches the end")

# ---------------------------------------------------------------------------
# Parser edge cases: no trailing newline, quoted separators, na = character()
# ---------------------------------------------------------------------------
# No trailing newline.
writeBin(charToRaw("a,b\n1,2"), tl_csv)
tl_parsed <- poorman:::csv_parse_file(tl_csv, TRUE, ",", "\"", "NA", "UTF-8")
expect_equal(length(tl_parsed$records), 1L, info = "file without trailing newline")
expect_equal(tl_parsed$records[[1L]], c("1", "2"))

# Separators inside quoted fields stay in the field; header-only file.
tl_write(c("a,b", '1,"x,y"'))
tl_parsed <- poorman:::csv_parse_file(tl_csv, TRUE, ",", "\"", "NA", "UTF-8")
expect_equal(tl_parsed$records[[1L]][2L], "x,y",
             info = "separator inside quotes is not a delimiter")
tl_write("a,b")
out <- poorman:::sqlite_csv_load(tl_csv, tl_db, "hdronly",
                                 col_types = c(a = "integer", b = "character"))
expect_equal(nrow(out), 0L, info = "header-only file loads zero rows")
expect_equal(poorman:::sqlite_csv_status(tl_csv, tl_db, "hdronly")$committed, 0L)

# na = character() means nothing is missing: even an unquoted empty field is
# literal empty text, and an unquoted NA is the literal string "NA".
tl_write(c("x,y", ",NA", '"" ,z'))
tl_parsed <- poorman:::csv_parse_file(tl_csv, TRUE, ",", "\"",
                                      character(0L), "UTF-8")
expect_equal(tl_parsed$records[[1L]][1L], "",
             info = "na = character() keeps unquoted empty as text")
expect_equal(tl_parsed$records[[1L]][2L], "NA",
             info = "na = character() keeps NA as literal text")
expect_true(!any(is.na(unlist(tl_parsed$records))),
            info = "no missing values when na is empty")

# Logical 0/FALSE and integer 0 survive a full load+resume round trip.
tl_write(c("i,l", "0,0", "1,FALSE", "2,TRUE"))
out <- poorman:::sqlite_csv_load(tl_csv, tl_db, "zeros",
            col_types = c(i = "integer", l = "logical"), batch_size = 1L)
expect_equal(out$i, c(0L, 1L, 2L), info = "integer zero through SQLite")
expect_equal(out$l, c(FALSE, FALSE, TRUE), info = "0/FALSE through SQLite")
out2 <- poorman:::sqlite_csv_load(tl_csv, tl_db, "zeros",
             col_types = c(i = "integer", l = "logical"), batch_size = 2L)
expect_equal(out, out2, info = "zero values stable across reopen")

# Integer out of range is rejected rather than silently coerced to NA.
tl_write(c("i", "99999999999999999999"))
poorman:::sqlite_csv_reset(tl_csv, tl_db, "bigint")
expect_error(
  poorman:::sqlite_csv_load(tl_csv, tl_db, "bigint",
                            col_types = c(i = "integer")),
  pattern = "out of range"
)

unlink(tl_wd, recursive = TRUE)
