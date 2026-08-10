#' Classify selected data-frame columns by analysis type
#'
#' Returns a type label for each selected column, preserving the supplied
#' column order. Numeric and integer columns are classified as `continuous`;
#' character, factor, ordered-factor, and logical columns as `categorical`;
#' and `Date` or `POSIXt` columns as `date`. Other column classes are labelled
#' `other`.
#'
#' @param dataset A data frame or tibble.
#' @param selected_columns A character vector containing column names from
#'   `dataset`.
#'
#' @return A named character vector with one type for each selected column.
#'
#' @examples
#' example_data <- data.frame(
#'   age = c(25, 40),
#'   group = c("A", "B"),
#'   visit_date = as.Date(c("2026-01-01", "2026-01-02")),
#'   enrolled = c(TRUE, FALSE)
#' )
#'
#' Moose_data_type(
#'   example_data,
#'   c("age", "group", "visit_date", "enrolled")
#' )
#'
#' @export
Moose_data_type <- function(dataset, selected_columns) {
  if (!is.data.frame(dataset)) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }

  if (
    !is.character(selected_columns) ||
      anyNA(selected_columns) ||
      any(!nzchar(selected_columns))
  ) {
    stop(
      "`selected_columns` must be a character vector of non-empty column names.",
      call. = FALSE
    )
  }

  missing_columns <- setdiff(selected_columns, names(dataset))

  if (length(missing_columns)) {
    stop(
      paste0(
        "Unknown `selected_columns`: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  classify_column <- function(column) {
    if (inherits(column, "Date") || inherits(column, "POSIXt")) {
      return("date")
    }

    if (is.numeric(column) || is.integer(column)) {
      return("continuous")
    }

    if (is.character(column) || is.factor(column) || is.logical(column)) {
      return("categorical")
    }

    "other"
  }

  output <- vapply(
    dataset[selected_columns],
    classify_column,
    character(1),
    USE.NAMES = FALSE
  )
  stats::setNames(output, selected_columns)
}
