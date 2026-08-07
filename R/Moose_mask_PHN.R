#' Mask Alberta Personal Health Numbers near a PHN label
#'
#' Finds the standalone label `PHN` case-insensitively and masks a nearby
#' nine-digit number. Both compact values such as `123456789` and hyphenated
#' values such as `12345-6789` are supported. The label itself is preserved.
#'
#' A number must occur on the same line, before or after `PHN`, with no other
#' digit between the label and the number. `proximity` controls the maximum
#' number of intervening non-digit characters.
#'
#' @param text A character or factor vector.
#' @param replacement A single non-missing character value used in place of
#'   each detected PHN. Defaults to `"[PHN]"`.
#' @param proximity A single non-negative integer giving the maximum number of
#'   non-digit characters allowed between `PHN` and the number.
#'
#' @return A character vector with detected PHNs replaced.
#'
#' @examples
#' Moose_mask_PHN(c(
#'   "PHN: 123456789",
#'   "phn 12345-6789",
#'   "123456789 (PHN)",
#'   "Unrelated number 123456789"
#' ))
#'
#' @export
Moose_mask_PHN <- function(text,
                           replacement = "[PHN]",
                           proximity = 20L) {
  if (is.factor(text)) {
    text <- as.character(text)
  }

  if (!is.character(text)) {
    stop("`text` must be a character or factor vector.", call. = FALSE)
  }

  if (
    !is.character(replacement) ||
      length(replacement) != 1L ||
      is.na(replacement)
  ) {
    stop("`replacement` must be one non-missing character value.", call. = FALSE)
  }

  proximity <- moose_validate_phn_proximity(proximity)

  output <- text

  for (row_id in seq_along(text)) {
    value <- text[row_id]

    if (is.na(value) || !nzchar(value)) {
      next
    }

    positions <- moose_phn_positions(value, proximity)

    if (!nrow(positions)) {
      next
    }

    positions <- positions[order(positions$start, decreasing = TRUE), , drop = FALSE]
    current <- value

    for (i in seq_len(nrow(positions))) {
      left <- if (positions$start[i] > 1L) {
        substr(current, 1L, positions$start[i] - 1L)
      } else {
        ""
      }
      right <- if (positions$end[i] < nchar(current)) {
        substr(current, positions$end[i] + 1L, nchar(current))
      } else {
        ""
      }
      current <- paste0(left, replacement, right)
    }

    output[row_id] <- current
  }

  output
}

#' Flag text containing an Alberta Personal Health Number
#'
#' Uses the same PHN-label proximity rules as [Moose_mask_PHN()] and returns
#' `1L` when a PHN is found or `0L` otherwise. Missing and blank values return
#' `0L`.
#'
#' @param text A character or factor vector.
#' @param proximity A single non-negative integer giving the maximum number of
#'   non-digit characters allowed between `PHN` and the number.
#'
#' @return An integer vector with the same length as `text`.
#'
#' @examples
#' Moose_mask_PHN_flag(c(
#'   "PHN: 123456789",
#'   "No PHN is present",
#'   NA_character_
#' ))
#'
#' @export
Moose_mask_PHN_flag <- function(text, proximity = 20L) {
  if (is.factor(text)) {
    text <- as.character(text)
  }

  if (!is.character(text)) {
    stop("`text` must be a character or factor vector.", call. = FALSE)
  }

  proximity <- moose_validate_phn_proximity(proximity)
  flag <- integer(length(text))
  candidates <- which(!is.na(text) & nzchar(text))

  if (!length(candidates)) {
    return(flag)
  }

  flag[candidates] <- vapply(
    text[candidates],
    function(value) as.integer(nrow(moose_phn_positions(value, proximity)) > 0L),
    integer(1)
  )
  flag
}

moose_validate_phn_proximity <- function(proximity) {
  if (
    !is.numeric(proximity) ||
      length(proximity) != 1L ||
      is.na(proximity) ||
      !is.finite(proximity) ||
      proximity < 0L ||
      proximity != as.integer(proximity)
  ) {
    stop("`proximity` must be one non-negative integer.", call. = FALSE)
  }

  as.integer(proximity)
}

moose_phn_patterns <- function(proximity) {
  number_pattern <- paste0(
    "(?<![0-9])(?:",
    "[0-9]{5}[[:space:]]*-[[:space:]]*[0-9]{4}",
    "|[0-9]{9}",
    ")(?![0-9])"
  )

  c(
    paste0(
      "(?i:\\bPHN\\b)[^0-9\\r\\n]{0,",
      proximity,
      "}\\K",
      number_pattern
    ),
    paste0(
      number_pattern,
      "(?=[^0-9\\r\\n]{0,",
      proximity,
      "}(?i:\\bPHN\\b))"
    )
  )
}

moose_phn_positions <- function(value, proximity) {
  positions <- lapply(moose_phn_patterns(proximity), function(pattern) {
    matches <- gregexpr(pattern, value, perl = TRUE)[[1]]

    if (identical(matches[1], -1L)) {
      return(NULL)
    }

    data.frame(
      start = as.integer(matches),
      end = as.integer(matches + attr(matches, "match.length") - 1L)
    )
  })
  positions <- Filter(Negate(is.null), positions)

  if (!length(positions)) {
    return(data.frame(start = integer(), end = integer()))
  }

  unique(do.call(rbind, positions))
}
