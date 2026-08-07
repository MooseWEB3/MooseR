#' One-row n(%) table for a categorical variable
#'
#' Builds a single, display-ready data frame row of counts and percentages
#' for each level of a categorical variable, plus a \code{Total} column.
#'
#' @param dataset A data frame containing \code{var_name}.
#' @param var_name String. The column to summarize.
#' @param display_name String. Label placed in the first column header.
#'   Defaults to \code{var_name}.
#' @param digits Integer. Decimal places for percentages (default 1).
#' @param include_missing Logical. If \code{TRUE} (default), NA values are counted
#'   under \code{missing_label}.
#' @param missing_label String used to label missing values (default "Missing").
#' @param sort_levels One of \code{"desc"} (default, by count desc),
#'   \code{"asc"}, \code{"alpha"} (alphabetical), or \code{"levels"} (keep factor order).
#' @param top_n Positive integer or \code{NULL}. If set, keeps the \code{top_n}
#'   most frequent levels, regardless of \code{sort_levels}, and collapses the
#'   rest into an \code{"Other"} column.
#' @param other_label String label used when collapsing to "Other" (default "Other").
#'
#' @return A one-row data frame: first column is \code{display_name},
#'   then one column per level showing \code{"n (p%)"}, ending with \code{Total}.
#'   Column names are made unique if labels collide.
#'
#' @examples
#' df <- data.frame(g = c("A","B","A", NA, "C","B","B"))
#' BD_1_cat(df, "g", display_name = "Group")
#' BD_1_cat(df, "g", digits = 0, sort_levels = "alpha", top_n = 2)
#'
#' @export
BD_1_cat <- function(dataset,
                     var_name,
                     display_name = var_name,
                     digits = 1,
                     include_missing = TRUE,
                     missing_label = "Missing",
                     sort_levels = c("desc", "asc", "alpha", "levels"),
                     top_n = NULL,
                     other_label = "Other") {
  sort_levels <- match.arg(sort_levels)

  validated <- moose_validate_summary_inputs(
    dataset,
    var_name,
    display_name,
    digits
  )
  var_name <- validated$var_name
  display_name <- validated$display_name
  digits <- validated$digits
  include_missing <- moose_validate_single_logical(
    include_missing,
    "include_missing"
  )
  missing_label <- moose_validate_summary_label(
    missing_label,
    "missing_label"
  )
  other_label <- moose_validate_summary_label(other_label, "other_label")
  top_n <- moose_validate_top_n(top_n)

  # Work on a copy of the target column.
  original_v <- dataset[[var_name]]
  v <- original_v

  # Add missing label if requested
  if (include_missing) {
    v <- as.character(v)
    v <- ifelse(is.na(v), missing_label, v)
  } else {
    v <- v[!is.na(v)]
  }

  # If after dropping/relabelling there's nothing left, return an empty row with Total 0
  if (length(v) == 0L) {
    out <- list("Count n(%)", paste0("0 (", sprintf(paste0("%.", digits, "f%%"), 0), ")"))
    names(out) <- c(display_name, "Total")
    return(data.frame(out, check.names = FALSE, stringsAsFactors = FALSE))
  }

  # Preserve factor level order if asked
  if (is.factor(original_v) && sort_levels == "levels") {
    # If missing_label inserted, ensure it's appended to levels
    lv <- levels(original_v)
    if (include_missing && !(missing_label %in% lv)) lv <- c(lv, missing_label)
    v <- factor(v, levels = lv)
  } else {
    # Otherwise treat as character for flexible sorting later
    v <- as.character(v)
  }

  # Compute counts using base R to avoid tidyverse runtime dependencies.
  counts_table <- table(v, useNA = "no")
  counts <- data.frame(
    val = names(counts_table),
    n = as.integer(counts_table),
    stringsAsFactors = FALSE
  )
  counts <- counts[counts$n > 0L, , drop = FALSE]

  # Select the most frequent levels before applying the requested display order.
  counts$is_other <- FALSE
  if (!is.null(top_n) && nrow(counts) > top_n) {
    frequency_order <- order(-counts$n, counts$val)
    keep <- frequency_order[seq_len(top_n)]
    head_part <- counts[keep, , drop = FALSE]
    other_n <- sum(counts$n[-keep])
    collapsed_label <- make.unique(
      c(head_part$val, other_label),
      sep = "_"
    )[[nrow(head_part) + 1L]]
    counts <- rbind(
      head_part,
      data.frame(
        val = collapsed_label,
        n = other_n,
        is_other = TRUE,
        stringsAsFactors = FALSE
      )
    )
  }

  regular_counts <- counts[!counts$is_other, , drop = FALSE]
  regular_counts <- switch(
    sort_levels,
    desc = regular_counts[
      order(-regular_counts$n, regular_counts$val),
      ,
      drop = FALSE
    ],
    asc = regular_counts[
      order(regular_counts$n, regular_counts$val),
      ,
      drop = FALSE
    ],
    alpha = regular_counts[order(regular_counts$val), , drop = FALSE],
    levels = {
      if (is.factor(v)) {
        level_index <- match(regular_counts$val, levels(v))
        regular_counts[order(level_index), , drop = FALSE]
      } else {
        regular_counts[
          order(-regular_counts$n, regular_counts$val),
          ,
          drop = FALSE
        ]
      }
    }
  )
  counts <- rbind(
    regular_counts,
    counts[counts$is_other, , drop = FALSE]
  )

  total_n <- sum(counts$n)

  pct_fmt <- function(n) sprintf(paste0("%.", digits, "f%%"), 100 * n / total_n)

  # Build display strings "n (p%)"
  counts$value <- paste0(counts$n, " (", pct_fmt(counts$n), ")")

  output_names <- make.unique(
    c(display_name, counts$val, "Total"),
    sep = "_"
  )
  row <- c(
    list("Count n(%)"),
    as.list(counts$value),
    list(paste0(total_n, " (", pct_fmt(total_n), ")"))
  )
  names(row) <- output_names

  data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
}
