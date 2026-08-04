#' Mask only names supplied in row-aligned columns
#'
#' Masks exact or similar spellings of names supplied by `name_columns` in the
#' same row of `data`. Unlike [Moose_mask_person_names()], this function does
#' not run spaCy, the general name regex engine, or non-person whitelists. Names
#' not represented by the same row's known-name columns are left unchanged.
#'
#' Exact matching is case-insensitive and supports individual fields,
#' `First Last`, and `Last, First` forms. Similar matching uses Levenshtein edit
#' distance on individual name tokens after removing accents and punctuation.
#' By default, one edit is allowed only for known names containing at least
#' five letters. Set `max_distance = 0L` to disable spelling-error tolerance.
#'
#' @param text A character or factor vector containing text to mask.
#' @param data A data frame with the same number of rows as `text`.
#' @param name_columns Character vector naming row-aligned known-name columns.
#' @param replacement Replacement text used for matched names.
#' @param max_distance Maximum Levenshtein edit distance, from 0 to 3.
#' @param min_chars Minimum normalized length of a known name eligible for
#'   similar matching. Exact matching is not restricted by this value.
#'
#' @return A character vector with the same length as `text`.
#'
#' @examples
#' records <- data.frame(
#'   comments = c("Jonathon Smyth called.", "Peter Brown called."),
#'   first_name = c("Jonathan", "Alice"),
#'   last_name = c("Smith", "Martin")
#' )
#' Moose_mask_person_names2(
#'   records$comments,
#'   data = records,
#'   name_columns = c("first_name", "last_name")
#' )
#'
#' @export
Moose_mask_person_names2 <- function(
    text,
    data,
    name_columns = c("first_name", "last_name"),
    replacement = "[NAME]",
    max_distance = 1L,
    min_chars = 5L) {
  if (is.factor(text)) {
    text <- as.character(text)
  }

  validate_name_masking_inputs(
    text = text,
    replacement = replacement,
    batch_size = 1L,
    keep_original = FALSE
  )
  validate_known_name_column_inputs(
    text = text,
    data = data,
    name_columns = name_columns
  )
  validate_known_name_similarity_inputs(max_distance, min_chars)

  if (!length(text)) {
    return(text)
  }

  output <- mask_known_name_columns(
    text = text,
    replacement = replacement,
    data = data,
    name_columns = name_columns
  )
  mask_similar_known_name_columns(
    text = output,
    replacement = replacement,
    data = data,
    name_columns = name_columns,
    max_distance = as.integer(max_distance),
    min_chars = as.integer(min_chars)
  )
}

#' Flag only names supplied in row-aligned columns
#'
#' Returns `1L` only when text contains an exact or similar spelling of a name
#' supplied by `name_columns` in the same row of `data`. It uses the same
#' restricted matching rules as [Moose_mask_person_names2()] and does not run
#' general personal-name detection.
#'
#' @inheritParams Moose_mask_person_names2
#'
#' @return An integer vector containing `1L` for matching rows and `0L` for
#'   non-matching, missing, or blank rows.
#'
#' @examples
#' records <- data.frame(
#'   comments = c("Jonathon Smyth called.", "Peter Brown called."),
#'   first_name = c("Jonathan", "Alice"),
#'   last_name = c("Smith", "Martin")
#' )
#' Moose_name_flag2(
#'   records$comments,
#'   data = records,
#'   name_columns = c("first_name", "last_name")
#' )
#'
#' @export
Moose_name_flag2 <- function(
    text,
    data,
    name_columns = c("first_name", "last_name"),
    max_distance = 1L,
    min_chars = 5L) {
  if (is.factor(text)) {
    text <- as.character(text)
  }

  validate_name_masking_inputs(
    text = text,
    replacement = "[NAME]",
    batch_size = 1L,
    keep_original = FALSE
  )
  validate_known_name_column_inputs(
    text = text,
    data = data,
    name_columns = name_columns
  )
  validate_known_name_similarity_inputs(max_distance, min_chars)

  if (!length(text)) {
    return(integer())
  }

  exact <- flag_known_name_columns(
    text = text,
    data = data,
    name_columns = name_columns
  )
  pending <- which(exact == 0L)

  if (!length(pending)) {
    return(exact)
  }

  similar <- flag_similar_known_name_columns(
    text = text[pending],
    data = data[pending, , drop = FALSE],
    name_columns = name_columns,
    max_distance = as.integer(max_distance),
    min_chars = as.integer(min_chars)
  )
  exact[pending] <- similar
  exact
}
