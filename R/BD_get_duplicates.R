#' Get duplicate rows by one or more keys
#'
#' Find duplicates based on selected columns. If no columns are selected,
#' duplicates are computed across all columns.
#'
#' @param data A data.frame or tibble.
#' @param ... Columns used as the duplicate keys, as unquoted names or strings.
#' @param .min_n Minimum frequency to keep (default 2).
#' @param .keep_all If TRUE, return all original rows with a count column.
#'   If FALSE, return one row per key with the count only.
#' @param na.rm If TRUE, drop rows where any key column is NA before counting.
#' @param .count_col Name of the count column to add (default ".n").
#' @param .sort If TRUE, sort descending by count (then keys).
#'
#' @return A data frame with duplicates and a count column.
#' @examples
#' # One key
#' BD_get_duplicates(mtcars, cyl)
#' # Multiple keys
#' BD_get_duplicates(mtcars, cyl, gear, .keep_all = FALSE)
#' # All columns (exact duplicate rows)
#' BD_get_duplicates(mtcars)
#' @export
BD_get_duplicates <- function(data, ...,
                              .min_n = 2,
                              .keep_all = TRUE,
                              na.rm = FALSE,
                              .count_col = ".n",
                              .sort = TRUE) {
  stopifnot(is.data.frame(data))

  dots <- as.list(substitute(list(...)))[-1L]
  flatten_keys <- function(expr) {
    if (is.call(expr) && identical(expr[[1L]], as.name("c"))) {
      unlist(lapply(as.list(expr)[-1L], flatten_keys), recursive = FALSE)
    } else {
      list(expr)
    }
  }

  key_exprs <- unlist(lapply(dots, flatten_keys), recursive = FALSE)
  keys <- if (length(key_exprs) == 0L) {
    names(data)
  } else {
    out <- character()
    for (expr in key_exprs) {
      if (is.name(expr)) {
        out <- c(out, as.character(expr))
      } else if (is.character(expr)) {
        out <- c(out, expr)
      } else {
        value <- eval(expr, parent.frame())
        if (is.character(value)) {
          out <- c(out, value)
        } else if (is.numeric(value)) {
          out <- c(out, names(data)[value])
        } else {
          stop("Columns in `...` must be unquoted names, strings, or numeric positions.", call. = FALSE)
        }
      }
    }
    unique(out)
  }

  if (!all(keys %in% names(data))) {
    missing_keys <- setdiff(keys, names(data))
    stop("Column(s) not found in `data`: ", paste(missing_keys, collapse = ", "), call. = FALSE)
  }

  dat <- data
  if (na.rm && length(keys) > 0) {
    dat <- dat[stats::complete.cases(dat[, keys, drop = FALSE]), , drop = FALSE]
  }

  same_key_row <- function(key_frame, one_row) {
    matched <- rep(TRUE, nrow(key_frame))
    for (key in names(key_frame)) {
      x <- key_frame[[key]]
      y <- one_row[[key]]
      if (is.factor(x)) x <- as.character(x)
      if (is.factor(y)) y <- as.character(y)
      matched <- matched & ((is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y))
    }
    matched
  }

  count_groups <- function(dat, keys) {
    if (length(keys) == 0L) {
      row_counts <- rep(nrow(dat), nrow(dat))
      counts <- data.frame(stringsAsFactors = FALSE)
      counts[[.count_col]] <- nrow(dat)
      return(list(counts = counts, row_counts = row_counts))
    }

    key_frame <- dat[, keys, drop = FALSE]
    if (nrow(key_frame) == 0L) {
      counts <- key_frame
      counts[[.count_col]] <- integer()
      return(list(counts = counts, row_counts = integer()))
    }

    unique_keys <- key_frame[!duplicated(key_frame), , drop = FALSE]
    group_counts <- integer(nrow(unique_keys))
    row_counts <- integer(nrow(key_frame))

    for (i in seq_len(nrow(unique_keys))) {
      matched <- same_key_row(key_frame, unique_keys[i, , drop = FALSE])
      group_counts[i] <- sum(matched)
      row_counts[matched] <- group_counts[i]
    }

    counts <- unique_keys
    counts[[.count_col]] <- group_counts
    list(counts = counts, row_counts = row_counts)
  }

  grouped <- count_groups(dat, keys)
  counts <- grouped$counts[grouped$counts[[.count_col]] >= .min_n, , drop = FALSE]

  if (.keep_all) {
    out <- dat
    out[[.count_col]] <- grouped$row_counts
    out[[.count_col]][out[[.count_col]] < .min_n] <- NA_integer_
  } else {
    out <- counts
  }

  if (.sort) {
    count_values <- out[[.count_col]]
    order_args <- c(
      list(is.na(count_values), -count_values),
      out[, keys, drop = FALSE],
      list(na.last = TRUE)
    )
    out <- out[do.call(order, order_args), , drop = FALSE]
  }

  row.names(out) <- NULL
  out
}
