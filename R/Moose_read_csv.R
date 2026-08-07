#' Import CSV files
#'
#' When `path` is a directory and `file_name` is `NULL`, imports every CSV file
#' in deterministic file-name order, assigns the results as
#' `imported_data_001`, `imported_data_002`, and so on in `envir`, and reports
#' each file-to-object mapping. The imported objects are also returned in an
#' invisible named list.
#'
#' The original single-file call `Moose_read_csv(path, file_name)` remains
#' supported and returns one data frame without assigning it. Passing a CSV
#' file directly as `path` has the same single-file behavior.
#'
#' @param path A directory to import, or a single file path.
#' @param file_name Optional file name within `path`, retained for backward
#'   compatibility. When supplied, only that file is returned.
#' @param prefix Prefix used for objects created during directory imports.
#' @param envir Environment receiving objects during directory imports.
#' @param recursive Logical. Search subdirectories during directory imports.
#' @param overwrite Logical. Allow existing object names in `envir` to be
#'   replaced. The default is `FALSE`.
#' @param clean_names Logical. Clean imported column names using MooseR's
#'   existing CSV-name rules.
#' @param ... Additional arguments passed to [utils::read.csv()].
#'
#' @return For a single file, the imported data frame. For a directory, an
#'   invisible named list containing all imported data frames.
#'
#' @examples
#' csv_dir <- tempfile()
#' dir.create(csv_dir)
#' writeLines("First Name,Value/Score\nAlice,10", file.path(csv_dir, "a.csv"))
#' imported <- new.env(parent = emptyenv())
#' Moose_read_csv(csv_dir, envir = imported)
#' imported$imported_data_001
#' unlink(csv_dir, recursive = TRUE)
#'
#' @export
Moose_read_csv <- function(path,
                           file_name = NULL,
                           prefix = "imported_data",
                           envir = parent.frame(),
                           recursive = FALSE,
                           overwrite = FALSE,
                           clean_names = TRUE,
                           ...) {
  validate_moose_clean_names(clean_names)
  single_file <- resolve_moose_import_path(path, file_name, "csv")
  reader <- function(file) {
    arguments <- utils::modifyList(
      list(
        file = file,
        check.names = FALSE,
        stringsAsFactors = FALSE
      ),
      list(...)
    )
    imported <- do.call(utils::read.csv, arguments)

    if (isTRUE(clean_names)) {
      names(imported) <- clean_moose_csv_names(names(imported))
    }

    imported
  }

  if (!is.null(single_file)) {
    return(reader(single_file))
  }

  import_moose_directory(
    path = path,
    extensions = "csv",
    reader = reader,
    prefix = prefix,
    envir = envir,
    recursive = recursive,
    overwrite = overwrite
  )
}

#' Import XLSX files
#'
#' Imports every XLSX workbook in a directory, assigns each selected worksheet
#' as `imported_data_001`, `imported_data_002`, and so on, and reports the
#' mapping. Files are processed in deterministic file-name order. Passing a
#' workbook directly as `path`, or supplying `file_name`, returns one imported
#' worksheet without assigning it.
#'
#' @inheritParams Moose_read_csv
#' @param sheet Sheet name or position passed to \code{readxl::read_excel()}. The
#'   default imports the first worksheet from every workbook.
#' @param ... Additional arguments passed to \code{readxl::read_excel()}.
#'
#' @return For a single file, the imported worksheet. For a directory, an
#'   invisible named list containing all imported worksheets.
#'
#' @examples
#' \dontrun{
#' Moose_read_xlsx("C:/data")
#' Moose_read_xlsx("C:/data", sheet = "Data")
#' }
#'
#' @export
Moose_read_xlsx <- function(path,
                            file_name = NULL,
                            prefix = "imported_data",
                            envir = parent.frame(),
                            recursive = FALSE,
                            overwrite = FALSE,
                            clean_names = TRUE,
                            sheet = 1,
                            ...) {
  validate_moose_clean_names(clean_names)

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "`Moose_read_xlsx()` requires the `readxl` package. Install it with install.packages(\"readxl\").",
      call. = FALSE
    )
  }

  single_file <- resolve_moose_import_path(path, file_name, "xlsx")
  reader <- function(file) {
    arguments <- utils::modifyList(
      list(path = file, sheet = sheet),
      list(...)
    )
    imported <- do.call(readxl::read_excel, arguments)

    if (isTRUE(clean_names)) {
      names(imported) <- clean_moose_csv_names(names(imported))
    }

    imported
  }

  if (!is.null(single_file)) {
    return(reader(single_file))
  }

  import_moose_directory(
    path = path,
    extensions = "xlsx",
    reader = reader,
    prefix = prefix,
    envir = envir,
    recursive = recursive,
    overwrite = overwrite
  )
}

#' Import RDS files
#'
#' Imports every RDS file in a directory, assigns the restored objects as
#' `imported_data_001`, `imported_data_002`, and so on, and reports the mapping.
#' Files are processed in deterministic file-name order. Passing an RDS file
#' directly as `path`, or supplying `file_name`, returns one restored object
#' without assigning it.
#'
#' @inheritParams Moose_read_csv
#' @param ... Additional arguments passed to [base::readRDS()].
#'
#' @return For a single file, the restored R object. For a directory, an
#'   invisible named list containing all restored objects.
#'
#' @examples
#' rds_dir <- tempfile()
#' dir.create(rds_dir)
#' saveRDS(data.frame(value = 1), file.path(rds_dir, "a.rds"))
#' imported <- new.env(parent = emptyenv())
#' Moose_read_rds(rds_dir, envir = imported)
#' imported$imported_data_001
#' unlink(rds_dir, recursive = TRUE)
#'
#' @export
Moose_read_rds <- function(path,
                           file_name = NULL,
                           prefix = "imported_data",
                           envir = parent.frame(),
                           recursive = FALSE,
                           overwrite = FALSE,
                           ...) {
  single_file <- resolve_moose_import_path(path, file_name, "rds")
  reader <- function(file) {
    arguments <- utils::modifyList(list(file = file), list(...))
    do.call(base::readRDS, arguments)
  }

  if (!is.null(single_file)) {
    return(reader(single_file))
  }

  import_moose_directory(
    path = path,
    extensions = "rds",
    reader = reader,
    prefix = prefix,
    envir = envir,
    recursive = recursive,
    overwrite = overwrite
  )
}

validate_moose_clean_names <- function(clean_names) {
  if (
    !is.logical(clean_names) ||
      length(clean_names) != 1L ||
      is.na(clean_names)
  ) {
    stop("`clean_names` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

validate_moose_import_options <- function(prefix,
                                          envir,
                                          recursive,
                                          overwrite) {
  if (
    !is.character(prefix) ||
      length(prefix) != 1L ||
      is.na(prefix) ||
      !nzchar(prefix) ||
      make.names(prefix) != prefix
  ) {
    stop("`prefix` must be one non-empty syntactic R name.", call. = FALSE)
  }

  if (!is.environment(envir)) {
    stop("`envir` must be an environment.", call. = FALSE)
  }

  for (value in list(recursive = recursive, overwrite = overwrite)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("`recursive` and `overwrite` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

resolve_moose_import_path <- function(path, file_name, extension) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    stop("`path` must be one non-empty file or directory path.", call. = FALSE)
  }

  target <- path

  if (!is.null(file_name)) {
    if (
      !is.character(file_name) ||
        length(file_name) != 1L ||
        is.na(file_name) ||
        !nzchar(file_name)
    ) {
      stop("`file_name` must be one non-empty file name.", call. = FALSE)
    }

    target <- file.path(path, file_name)
  }

  if (file.exists(target) && !dir.exists(target)) {
    actual_extension <- tolower(tools::file_ext(target))

    if (!identical(actual_extension, extension)) {
      stop(
        paste0("Expected a .", extension, " file: ", target),
        call. = FALSE
      )
    }

    return(normalizePath(target, winslash = "/", mustWork = TRUE))
  }

  if (!is.null(file_name)) {
    stop(paste0("File does not exist: ", target), call. = FALSE)
  }

  if (!dir.exists(path)) {
    stop(paste0("Directory does not exist: ", path), call. = FALSE)
  }

  NULL
}

list_moose_import_files <- function(path, extensions, recursive) {
  pattern <- paste0("\\.(?:", paste(extensions, collapse = "|"), ")$")
  files <- list.files(
    path = path,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )
  files <- files[file.exists(files) & !dir.exists(files)]
  files[order(tolower(basename(files)), tolower(files))]
}

import_moose_directory <- function(path,
                                   extensions,
                                   reader,
                                   prefix,
                                   envir,
                                   recursive,
                                   overwrite) {
  validate_moose_import_options(prefix, envir, recursive, overwrite)
  files <- list_moose_import_files(path, extensions, recursive)

  if (!length(files)) {
    stop(
      paste0(
        "No ",
        toupper(paste(extensions, collapse = " or ")),
        " files were found in: ",
        path
      ),
      call. = FALSE
    )
  }

  object_names <- sprintf("%s_%03d", prefix, seq_along(files))
  conflicts <- vapply(
    object_names,
    exists,
    logical(1),
    envir = envir,
    inherits = FALSE
  )

  if (any(conflicts) && !isTRUE(overwrite)) {
    stop(
      paste0(
        "Objects already exist in `envir`: ",
        paste(object_names[conflicts], collapse = ", "),
        ". Use `overwrite = TRUE` or a different `prefix`."
      ),
      call. = FALSE
    )
  }

  imported <- lapply(files, reader)
  names(imported) <- object_names
  list2env(imported, envir = envir)

  for (i in seq_along(files)) {
    message(basename(files[i]), " is imported as ", object_names[i])
  }

  invisible(imported)
}

clean_moose_csv_names <- function(x) {
  x <- gsub(" ", "_", x, fixed = TRUE)
  x <- gsub("/", "_", x, fixed = TRUE)
  x <- gsub("[(),]", "", x)
  x <- gsub("-", "", x, fixed = TRUE)
  x
}
