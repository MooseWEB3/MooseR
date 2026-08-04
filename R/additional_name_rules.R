#' Apply supplementary name-masking rules
#'
#' Applies simple regular-expression rules that can catch some title-based or
#' workflow-based names that named-entity recognition may miss.
#'
#' @param text A character vector.
#' @param replacement Replacement string.
#'
#' @return A character vector.
#'
#' @export
apply_name_masking_rules <- function(text, replacement = "[NAME]") {
  if (!is.character(text)) {
    stop("`text` must be a character vector.", call. = FALSE)
  }

  if (!is.character(replacement) || length(replacement) != 1L || is.na(replacement)) {
    stop("`replacement` must be one non-missing character value.", call. = FALSE)
  }

  # Comma-separated names such as "Smith, John".
  output <- mask_name_regex_matches(
    text = text,
    pattern = name_last_first_regex_pattern(),
    replacement = replacement
  )

  # Titles followed by likely names, such as "Dr. J. Brown" or "RN Johnson".
  output <- mask_name_regex_matches(
    text = output,
    pattern = name_title_regex_pattern(),
    replacement = replacement
  )

  # Workflow phrases such as "Reviewed by Firstname Lastname".
  output <- mask_name_regex_matches(
    text = output,
    pattern = name_workflow_regex_pattern(),
    replacement = replacement
  )

  output
}

mask_name_regex_matches <- function(text, pattern, replacement) {
  output <- text

  for (row_id in seq_along(text)) {
    value <- text[row_id]

    if (is.na(value) || !nzchar(trimws(value))) {
      next
    }

    matches <- gregexpr(pattern, value, perl = TRUE)[[1]]

    if (identical(matches[1], -1L)) {
      next
    }

    starts <- as.integer(matches)
    lengths <- attr(matches, "match.length")
    ends <- starts + lengths - 1L
    keep <- !mapply(
      is_nonperson_name_candidate,
      text = value,
      start = starts,
      end = ends,
      USE.NAMES = FALSE
    )

    if (!any(keep)) {
      next
    }

    starts <- starts[keep]
    ends <- ends[keep]
    current <- value

    for (i in order(starts, decreasing = TRUE)) {
      left <- if (starts[i] > 1L) {
        substr(current, 1L, starts[i] - 1L)
      } else {
        ""
      }
      right <- if (ends[i] < nchar(current)) {
        substr(current, ends[i] + 1L, nchar(current))
      } else {
        ""
      }
      current <- paste0(left, replacement, right)
    }

    output[row_id] <- current
  }

  output
}
