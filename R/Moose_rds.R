#' Save and load R objects in RDS format
#'
#' `Moose_SRds()` saves one R object to an RDS file. If `file` is omitted, the
#' object name is used as the file name. `Moose_LRds()` restores the object
#' without changing its class or attributes. Both functions add the `.rds`
#' extension when it is omitted.
#'
#' @param data Any R object to save.
#' @param file A single file path. In `Moose_SRds()`, the object name is used
#'   when `file` is `NULL`.
#' @param compress Logical or one of `"gzip"`, `"bzip2"`, or `"xz"`. Controls
#'   compression passed to [base::saveRDS()].
#' @param overwrite Logical. If `FALSE`, stop rather than replace an existing
#'   file.
#'
#' @return `Moose_SRds()` invisibly returns the normalized path of the saved
#'   file. `Moose_LRds()` returns the restored R object.
#'
#' @examples
#' rds_file <- tempfile(fileext = ".rds")
#' saved_path <- Moose_SRds(iris, rds_file)
#' restored <- Moose_LRds(saved_path)
#' identical(iris, restored)
#' unlink(saved_path)
#'
#' @export
Moose_SRds <- function(data,
                       file = NULL,
                       compress = TRUE,
                       overwrite = TRUE) {
  if (is.null(file)) {
    object_name <- deparse(substitute(data), nlines = 1L)

    if (length(object_name) != 1L ||
        !grepl("^[[:alpha:].][[:alnum:]_.]*$", object_name)) {
      object_name <- "MooseR_data"
    }

    file <- object_name
  }

  file <- moose_rds_path(file)
  overwrite <- moose_rds_validate_flag(overwrite, "overwrite")
  moose_rds_validate_compress(compress)

  directory <- dirname(file)
  if (!dir.exists(directory)) {
    stop(
      "The destination directory does not exist: ",
      directory,
      call. = FALSE
    )
  }

  if (file.exists(file) && !isTRUE(overwrite)) {
    stop(
      "The RDS file already exists. Use `overwrite = TRUE` to replace it: ",
      file,
      call. = FALSE
    )
  }

  saveRDS(data, file = file, compress = compress)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

#' @rdname Moose_SRds
#' @export
Moose_LRds <- function(file) {
  file <- moose_rds_path(file)

  if (!file.exists(file) || dir.exists(file)) {
    stop("The RDS file does not exist: ", file, call. = FALSE)
  }

  readRDS(file)
}

moose_rds_path <- function(file) {
  if (!is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(trimws(file))) {
    stop("`file` must be one non-empty character path.", call. = FALSE)
  }

  file <- path.expand(trimws(file))

  if (!grepl("[.]rds$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".rds")
  }

  file
}

moose_rds_validate_flag <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", argument, "` must be TRUE or FALSE.", call. = FALSE)
  }

  x
}

moose_rds_validate_compress <- function(compress) {
  valid_logical <- is.logical(compress) &&
    length(compress) == 1L &&
    !is.na(compress)
  valid_character <- is.character(compress) &&
    length(compress) == 1L &&
    !is.na(compress) &&
    compress %in% c("gzip", "bzip2", "xz")

  if (!valid_logical && !valid_character) {
    stop(
      "`compress` must be TRUE, FALSE, \"gzip\", \"bzip2\", or \"xz\".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
