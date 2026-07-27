#' Flag rows containing a detected personal name
#'
#' Uses the same spaCy or pure R regex engine as
#' [Moose_detect_person_names()] to inspect one text column. A new integer
#' column is added to a copy of the input data: `1L` means at least one personal
#' name was detected in that row, and `0L` means no name was detected. Missing
#' and blank text values receive `0L`.
#'
#' @param data A data frame or data-frame subclass.
#' @param column Text column to inspect. Supply a bare column name or a single
#'   character column name.
#' @param flag_column Name of the output flag column.
#' @param batch_size Number of documents processed per spaCy batch.
#' @param engine Character. One of `"auto"`, `"spacy"`, or `"regex"`.
#'
#' @return A copy of `data` with an integer flag column added. An existing
#'   column named by `flag_column` is replaced.
#'
#' @examples
#' example_data <- data.frame(
#'   comment = c("John Smith reviewed the file.", "No name is included.", NA)
#' )
#'
#' Moose_name_flag(example_data, comment, engine = "regex")
#' Moose_name_flag(
#'   example_data,
#'   "comment",
#'   flag_column = "has_name",
#'   engine = "regex"
#' )
#'
#' @export
Moose_name_flag <- function(data,
                            column,
                            flag_column = "name_flag",
                            batch_size = 100L,
                            engine = c("auto", "spacy", "regex")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or data-frame subclass.", call. = FALSE)
  }

  if (missing(column)) {
    stop("`column` must identify one text column in `data`.", call. = FALSE)
  }

  column_name <- moose_name_flag_resolve_column(
    data,
    substitute(column),
    parent.frame()
  )
  flag_column <- moose_name_flag_validate_output_name(flag_column)
  engine <- match.arg(engine)

  if (identical(column_name, flag_column)) {
    stop(
      "`flag_column` must be different from the inspected text column.",
      call. = FALSE
    )
  }

  text <- data[[column_name]]
  if (is.factor(text)) {
    text <- as.character(text)
  }

  if (!is.character(text)) {
    stop(
      "The selected column `",
      column_name,
      "` must be a character or factor column.",
      call. = FALSE
    )
  }

  detected <- detect_person_names(
    text,
    batch_size = batch_size,
    engine = engine
  )

  flag <- integer(nrow(data))

  if (nrow(detected)) {
    detected_rows <- unique(detected$row_id)
    detected_rows <- detected_rows[
      !is.na(detected_rows) &
        detected_rows >= 1L &
        detected_rows <= nrow(data)
    ]
    flag[detected_rows] <- 1L
  }

  output <- data
  output[[flag_column]] <- flag
  output
}

moose_name_flag_resolve_column <- function(data, column_expression, environment) {
  if (is.character(column_expression) && length(column_expression) == 1L) {
    column_name <- column_expression
  } else if (is.symbol(column_expression)) {
    symbol_name <- as.character(column_expression)

    if (symbol_name %in% names(data)) {
      column_name <- symbol_name
    } else {
      column_name <- tryCatch(
        eval(column_expression, envir = environment),
        error = function(error) NULL
      )
    }
  } else {
    column_name <- tryCatch(
      eval(column_expression, envir = environment),
      error = function(error) NULL
    )
  }

  if (!is.character(column_name) ||
      length(column_name) != 1L ||
      is.na(column_name) ||
      !nzchar(column_name)) {
    stop(
      "`column` must be a bare column name or one character column name.",
      call. = FALSE
    )
  }

  matches <- which(names(data) == column_name)
  if (length(matches) == 0L) {
    stop("Column `", column_name, "` was not found in `data`.", call. = FALSE)
  }

  if (length(matches) > 1L) {
    stop(
      "Column `",
      column_name,
      "` is duplicated in `data` and is ambiguous.",
      call. = FALSE
    )
  }

  column_name
}

moose_name_flag_validate_output_name <- function(flag_column) {
  if (!is.character(flag_column) ||
      length(flag_column) != 1L ||
      is.na(flag_column) ||
      !nzchar(trimws(flag_column))) {
    stop("`flag_column` must be one non-empty character name.", call. = FALSE)
  }

  trimws(flag_column)
}
