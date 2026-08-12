#' Create a two-variable cross table
#'
#' Builds a display-ready contingency table for two categorical variables.
#' Cells contain counts alone or counts with row, column, or total percentages.
#'
#' @param dataset A data frame or tibble.
#' @param row_var A single column name for table rows.
#' @param col_var A single column name for table columns.
#' @param percent One of `"row"`, `"column"`, `"total"`, or `"none"`.
#' @param include_missing Logical. Show missing values as a category.
#' @param include_total Logical. Add row and column totals.
#' @param digits Percentage decimal places.
#' @param missing_label Label used for missing values.
#' @return A display-ready data frame.
#' @examples
#' d <- data.frame(sex = c("F", "F", "M"), group = c("A", "B", "A"))
#' Moose_cross_table(d, "sex", "group")
#' Moose_cross_table(d, "sex", "group", percent = "column")
#' @export
Moose_cross_table <- function(dataset, row_var, col_var,
                              percent = c("row", "column", "total", "none"),
                              include_missing = TRUE, include_total = TRUE,
                              digits = 1L, missing_label = "Missing") {
  if (!is.data.frame(dataset)) stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  check_column <- function(x, arg) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
      stop("`", arg, "` must be one non-empty column name.", call. = FALSE)
    if (!x %in% names(dataset)) stop("Unknown `", arg, "`: ", x, ".", call. = FALSE)
    x
  }
  row_var <- check_column(row_var, "row_var")
  col_var <- check_column(col_var, "col_var")
  percent <- match.arg(percent)
  check_flag <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x))
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    x
  }
  include_missing <- check_flag(include_missing, "include_missing")
  include_total <- check_flag(include_total, "include_total")
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      !is.finite(digits) || digits < 0 || digits != as.integer(digits))
    stop("`digits` must be one non-negative integer.", call. = FALSE)
  digits <- as.integer(digits)
  if (!is.character(missing_label) || length(missing_label) != 1L ||
      is.na(missing_label) || !nzchar(missing_label))
    stop("`missing_label` must be one non-empty character value.", call. = FALSE)

  prepare <- function(x) {
    lev <- if (is.factor(x)) levels(x) else unique(as.character(x[!is.na(x)]))
    x <- as.character(x)
    if (include_missing) x[is.na(x)] <- missing_label
    if (include_missing && any(x == missing_label)) lev <- c(lev, missing_label)
    factor(x, levels = unique(lev))
  }
  r <- prepare(dataset[[row_var]])
  c <- prepare(dataset[[col_var]])
  keep <- if (include_missing) rep(TRUE, nrow(dataset)) else !is.na(r) & !is.na(c)
  n <- table(r[keep], c[keep], useNA = "no")

  p <- switch(percent,
    row = sweep(n, 1L, rowSums(n), "/"),
    column = sweep(n, 2L, colSums(n), "/"),
    total = n / sum(n), none = NULL)
  if (!is.null(p)) p[!is.finite(p)] <- 0
  fmt <- function(count, pct = NULL) {
    if (is.null(pct)) return(as.character(count))
    paste0(count, " (", sprintf(paste0("%.", digits, "f%%"), 100 * pct), ")")
  }
  displayed <- if (percent == "none")
    matrix(fmt(as.vector(n)), nrow(n), dimnames = dimnames(n)) else
    matrix(fmt(as.vector(n), as.vector(p)), nrow(n), dimnames = dimnames(n))

  if (include_total) {
    rt <- rowSums(n); ct <- colSums(n); gt <- sum(n)
    rtp <- switch(percent, row = ifelse(rt > 0, 1, 0),
      column = if (gt > 0) rt / gt else rep(0, length(rt)),
      total = if (gt > 0) rt / gt else rep(0, length(rt)), none = NULL)
    displayed <- cbind(displayed, Total = fmt(rt, rtp))
    ctp <- switch(percent, row = if (gt > 0) ct / gt else rep(0, length(ct)),
      column = ifelse(ct > 0, 1, 0),
      total = if (gt > 0) ct / gt else rep(0, length(ct)), none = NULL)
    bottom <- c(fmt(ct, ctp), fmt(gt, if (percent == "none") NULL else 1))
    displayed <- rbind(displayed, Total = bottom)
  }
  out <- data.frame(row_category = rownames(displayed), displayed,
                    check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[1L] <- row_var
  row.names(out) <- NULL
  out
}
