library(MooseR)

local({
  root <- tempfile("mooser-import-")
  csv_dir <- file.path(root, "csv")
  rds_dir <- file.path(root, "rds")
  empty_dir <- file.path(root, "empty")
  dir.create(csv_dir, recursive = TRUE)
  dir.create(rds_dir, recursive = TRUE)
  dir.create(empty_dir, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  writeLines(
    "First Name,Value/Score\nAlice,10",
    file.path(csv_dir, "a.CSV")
  )
  writeLines(
    "Second Name,Value-Score\nBob,20",
    file.path(csv_dir, "b.csv")
  )
  writeLines("ignored", file.path(csv_dir, "ignore.txt"))

  csv_environment <- new.env(parent = emptyenv())
  csv_messages <- capture.output(
    csv_result <- Moose_read_csv(csv_dir, envir = csv_environment),
    type = "message"
  )

  stopifnot(
    identical(names(csv_result), c("imported_data_001", "imported_data_002")),
    identical(
      csv_messages,
      c(
        "a.CSV is imported as imported_data_001",
        "b.csv is imported as imported_data_002"
      )
    ),
    identical(
      names(csv_environment$imported_data_001),
      c("First_Name", "Value_Score")
    ),
    identical(csv_environment$imported_data_001$First_Name, "Alice"),
    identical(csv_environment$imported_data_002$Second_Name, "Bob"),
    identical(
      Moose_read_csv(csv_dir, "a.CSV"),
      csv_environment$imported_data_001
    ),
    identical(
      Moose_read_csv(file.path(csv_dir, "a.CSV")),
      csv_environment$imported_data_001
    ),
    inherits(
      tryCatch(
        Moose_read_csv(csv_dir, envir = csv_environment),
        error = identity
      ),
      "error"
    ),
    inherits(
      tryCatch(Moose_read_csv(empty_dir), error = identity),
      "error"
    )
  )

  saveRDS(data.frame(value = 1L), file.path(rds_dir, "a.rds"))
  saveRDS(list(value = 2L), file.path(rds_dir, "b.RDS"))
  rds_environment <- new.env(parent = emptyenv())
  rds_messages <- capture.output(
    rds_result <- Moose_read_rds(rds_dir, envir = rds_environment),
    type = "message"
  )

  stopifnot(
    identical(names(rds_result), c("imported_data_001", "imported_data_002")),
    identical(
      rds_messages,
      c(
        "a.rds is imported as imported_data_001",
        "b.RDS is imported as imported_data_002"
      )
    ),
    identical(rds_environment$imported_data_001$value, 1L),
    identical(rds_environment$imported_data_002$value, 2L),
    identical(
      Moose_read_rds(file.path(rds_dir, "b.RDS")),
      list(value = 2L)
    )
  )

  if (!requireNamespace("readxl", quietly = TRUE)) {
    xlsx_error <- tryCatch(
      Moose_read_xlsx(empty_dir),
      error = identity
    )
    stopifnot(
      inherits(xlsx_error, "error"),
      grepl("requires the `readxl` package", conditionMessage(xlsx_error), fixed = TRUE)
    )
  }
})
