#' Check if a column is unique
#'
#' Reports whether a column has unique values (optionally ignoring NAs),
#' along with counts and the set of duplicated values.
#'
#' @param data A data.frame or tibble.
#' @param var  Column to check; unquoted name or a string.
#' @param na.rm If TRUE, ignore NA values when assessing uniqueness.
#'
#' @return A list with elements:
#' \itemize{
#'   \item \code{variable}: column name as a string
#'   \item \code{n_unique}: number of distinct values (per \code{na.rm})
#'   \item \code{n_rows}: total rows in \code{data}
#'   \item \code{n_non_na}: number of non-NA entries in \code{var}
#'   \item \code{is_unique}: TRUE if all (non-NA if \code{na.rm=TRUE}) values are unique
#'   \item \code{duplicates}: vector of values that appear more than once
#' }
#'
#' @examples
#' BD_check_unique(iris, Species)
#' BD_check_unique(iris, "Species", na.rm = TRUE)
#'
#' @export
BD_check_unique <- function(data, var, na.rm = FALSE) {
  stopifnot(is.data.frame(data))

  var_expr <- substitute(var)
  if (is.name(var_expr)) {
    col_name <- as.character(var_expr)
  } else if (is.character(var_expr) && length(var_expr) == 1L) {
    col_name <- var_expr
  } else {
    var_value <- eval(var_expr, parent.frame())
    if (!is.character(var_value) || length(var_value) != 1L) {
      stop("`var` must be an unquoted column name or a single column name string.", call. = FALSE)
    }
    col_name <- var_value
  }

  if (!col_name %in% names(data)) {
    stop(sprintf("Column `%s` not found in `data`.", col_name), call. = FALSE)
  }

  col <- data[[col_name]]
  n_rows   <- nrow(data)
  n_non_na <- sum(!is.na(col))
  vals_for_unique <- if (na.rm) col[!is.na(col)] else col
  n_unique <- length(unique(vals_for_unique))

  # Determine uniqueness criterion
  is_unique <- if (na.rm) {
    n_unique == n_non_na
  } else {
    n_unique == n_rows
  }

  # Identify duplicate values (respecting na.rm)
  vals <- if (na.rm) col[!is.na(col)] else col
  dup_vals <- unique(vals[duplicated(vals)])

  list(
    variable  = col_name,
    n_unique  = n_unique,
    n_rows    = n_rows,
    n_non_na  = n_non_na,
    is_unique = is_unique,
    duplicates = dup_vals
  )
}
