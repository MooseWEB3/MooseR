#' Mask vector values from the front or back
#'
#' Converts an atomic vector or factor to character and replaces characters
#' with `"*"`. By default, every character in each non-missing value is
#' masked. Supply `n` to mask only a specified number of characters from the
#' `front` or `back`.
#'
#' Masking is applied by Unicode grapheme cluster, so a user-perceived
#' character such as a letter plus a combining accent is not split. Spaces,
#' punctuation, signs, and decimal points are treated as characters and are
#' masked in the same way as letters and digits.
#'
#' Partial masking preserves part of the original value, and full masking
#' still reveals value length and missingness. This function is not encryption
#' and does not by itself anonymize or de-identify a data set.
#' The input vector's `names()` are preserved unchanged; remove or mask them
#' separately if they contain sensitive information. Setting `n = 0` returns
#' the character representation of the original values without masking them.
#'
#' Numeric identifiers that originally contained leading zeroes cannot recover
#' those zeroes after conversion to numeric. Store such identifiers as
#' character before calling this function.
#'
#' @param input An atomic vector or factor, typically a data-frame column.
#'   Matrices, arrays, lists, and data frames are not supported.
#' @param side Side from which masking starts when `n` is supplied. Must be
#'   either `"front"` or `"back"`.
#' @param n `NULL` to mask every character (the default), or one non-negative
#'   whole number giving the number of Unicode grapheme clusters to mask. Zero
#'   leaves the character representation unchanged. If `n` is greater than or
#'   equal to a string's grapheme-cluster count, that string is fully masked.
#'
#' @return A character vector with the same length, order, and names as
#'   `input`. Missing values remain `NA_character_`; other attributes and
#'   classes are not retained.
#'
#' @examples
#' ids <- c("1023056", "98765", NA)
#'
#' Moose_mask(ids)
#' Moose_mask(ids, side = "front", n = 3)
#' Moose_mask(ids, side = "back", n = 3)
#'
#' Moose_mask(1023056, "front", 3)
#'
#' @export
Moose_mask <- function(input, side = "front", n = NULL) {
  if (is.null(input) || !is.atomic(input) || !is.null(dim(input))) {
    stop("`input` must be an atomic vector or factor.", call. = FALSE)
  }

  if (
    !is.character(side) ||
      length(side) != 1L ||
      is.na(side) ||
      !side %in% c("front", "back")
  ) {
    stop("`side` must be either \"front\" or \"back\".", call. = FALSE)
  }

  if (!is.null(n)) {
    valid_n <-
      is.numeric(n) &&
      !is.logical(n) &&
      length(n) == 1L &&
      !is.na(n) &&
      is.finite(n) &&
      n >= 0 &&
      n == floor(n)

    if (!valid_n) {
      stop("`n` must be NULL or one non-negative whole number.", call. = FALSE)
    }
  }

  input_names <- names(input)
  missing <- tryCatch(
    is.na(input),
    error = function(error) {
      stop("`input` could not be converted safely.", call. = FALSE)
    }
  )
  output <- tryCatch(
    as.character(input),
    error = function(error) {
      stop("`input` could not be converted safely.", call. = FALSE)
    }
  )

  if (length(missing) != length(input) || length(output) != length(input)) {
    stop("`input` could not be converted safely.", call. = FALSE)
  }

  output[missing] <- NA_character_

  candidates <- which(!missing)
  if (length(candidates)) {
    output[candidates] <- vapply(
      output[candidates],
      moose_mask_one,
      character(1),
      side = side,
      n = n,
      USE.NAMES = FALSE
    )
  }

  names(output) <- input_names
  output
}

moose_mask_one <- function(value, side, n) {
  if (!nzchar(value)) {
    return("")
  }

  positions <- gregexpr("\\X", value, perl = TRUE)[[1L]]
  graphemes <- regmatches(value, list(positions))[[1L]]
  value_length <- length(graphemes)

  if (!value_length) {
    return("")
  }

  mask_count <- if (is.null(n)) {
    value_length
  } else {
    min(n, value_length)
  }

  if (mask_count == 0) {
    return(value)
  }

  if (mask_count == value_length) {
    return(strrep("*", value_length))
  }

  if (side == "front") {
    graphemes[seq_len(mask_count)] <- "*"
  } else {
    graphemes[seq.int(value_length - mask_count + 1, value_length)] <- "*"
  }

  paste0(graphemes, collapse = "")
}
