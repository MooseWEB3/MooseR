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

  output <- text

  # Titles followed by likely names, such as "Dr. J. Brown" or "RN Johnson".
  output <- gsub(
    pattern = name_title_regex_pattern(),
    replacement = replacement,
    x = output,
    perl = TRUE
  )

  # Workflow phrases such as "Reviewed by Firstname Lastname".
  output <- gsub(
    pattern = name_workflow_regex_pattern(),
    replacement = replacement,
    x = output,
    perl = TRUE
  )

  output
}
