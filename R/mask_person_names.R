#' Detect and mask personal names in text
#'
#' Uses spaCy named-entity recognition when available. On locked-down
#' computers where Python cannot be initialized, \code{engine = "auto"} falls
#' back to a pure R regular-expression engine.
#'
#' @param text A character vector.
#' @param replacement Replacement text used for detected names.
#' @param batch_size Number of documents processed in each spaCy batch.
#' @param keep_original Logical. If \code{TRUE}, return a data frame containing
#'   both original and masked text.
#' @param engine Character. One of \code{"auto"}, \code{"spacy"}, or
#'   \code{"regex"}.
#' @param apply_rules Logical. If \code{TRUE}, apply supplementary regex rules
#'   after spaCy or regex masking.
#'
#' @return A character vector, or a data frame when
#'   \code{keep_original = TRUE}.
#'
#' @examples
#' \dontrun{
#' setup_name_masking()
#'
#' mask_person_names(
#'   c(
#'     "John Smith spoke with Sarah Johnson.",
#'     "No personal name is included here."
#'   )
#' )
#' }
#'
#' @export
mask_person_names <- function(text,
                              replacement = "[NAME]",
                              batch_size = 100L,
                              keep_original = FALSE,
                              engine = c("auto", "spacy", "regex"),
                              apply_rules = TRUE) {
  validate_name_masking_inputs(text, replacement, batch_size, keep_original)
  engine <- match.arg(engine)

  if (!is.logical(apply_rules) || length(apply_rules) != 1L || is.na(apply_rules)) {
    stop("`apply_rules` must be TRUE or FALSE.", call. = FALSE)
  }

  if (length(text) == 0L) {
    return(text)
  }

  state <- get_name_masking_state()

  if (
    !isTRUE(state$setup_complete) ||
      (engine == "spacy" && !identical(state$engine, "spacy")) ||
      (engine == "regex" && !identical(state$engine, "regex"))
  ) {
    setup_name_masking(engine = engine)
    state <- get_name_masking_state()
  }

  if (!identical(state$engine, "spacy") || is.null(state$model)) {
    output <- mask_person_names_regex(
      text = text,
      replacement = replacement,
      apply_rules = apply_rules
    )

    if (isTRUE(keep_original)) {
      return(
        data.frame(
          original_text = text,
          masked_text = output,
          stringsAsFactors = FALSE
        )
      )
    }

    return(output)
  }

  model <- state$model

  missing_input <- is.na(text)
  blank_input <- !missing_input & !nzchar(trimws(text))
  process_input <- !missing_input & !blank_input

  output <- text

  if (any(process_input)) {
    input_text <- text[process_input]

    python_text <- reticulate::r_to_py(
      as.list(input_text),
      convert = FALSE
    )

    documents <- model$pipe(
      python_text,
      batch_size = as.integer(batch_size)
    )

    document_list <- reticulate::iterate(
      documents,
      simplify = FALSE
    )

    masked_values <- vapply(
      document_list,
      mask_single_spacy_document,
      replacement = replacement,
      FUN.VALUE = character(1)
    )

    output[process_input] <- masked_values
  }

  if (isTRUE(apply_rules)) {
    output <- apply_name_masking_rules(output, replacement = replacement)
  }

  if (isTRUE(keep_original)) {
    return(
      data.frame(
        original_text = text,
        masked_text = output,
        stringsAsFactors = FALSE
      )
    )
  }

  output
}

#' Detect personal names without modifying the text
#'
#' @param text A character vector.
#' @param batch_size Number of documents processed per batch.
#' @param engine Character. One of \code{"auto"}, \code{"spacy"}, or
#'   \code{"regex"}.
#'
#' @return A data frame containing document number, detected name, and
#'   character offsets.
#'
#' @export
detect_person_names <- function(text,
                                batch_size = 100L,
                                engine = c("auto", "spacy", "regex")) {
  validate_name_masking_inputs(
    text = text,
    replacement = "[NAME]",
    batch_size = batch_size,
    keep_original = FALSE
  )
  engine <- match.arg(engine)

  empty_result <- data.frame(
    row_id = integer(),
    detected_name = character(),
    start = integer(),
    end = integer(),
    stringsAsFactors = FALSE
  )

  valid_indices <- which(!is.na(text) & nzchar(trimws(text)))

  if (length(valid_indices) == 0L) {
    return(empty_result)
  }

  state <- get_name_masking_state()

  if (
    !isTRUE(state$setup_complete) ||
      (engine == "spacy" && !identical(state$engine, "spacy")) ||
      (engine == "regex" && !identical(state$engine, "regex"))
  ) {
    setup_name_masking(engine = engine)
    state <- get_name_masking_state()
  }

  if (!identical(state$engine, "spacy") || is.null(state$model)) {
    return(detect_person_names_regex(text))
  }

  model <- state$model

  documents <- model$pipe(
    reticulate::r_to_py(
      as.list(text[valid_indices]),
      convert = FALSE
    ),
    batch_size = as.integer(batch_size)
  )

  document_list <- reticulate::iterate(
    documents,
    simplify = FALSE
  )

  result <- vector(
    mode = "list",
    length = length(document_list)
  )

  for (i in seq_along(document_list)) {
    entities <- reticulate::iterate(
      document_list[[i]]$ents,
      simplify = FALSE
    )

    entities <- Filter(
      function(entity) {
        identical(
          reticulate::py_to_r(entity$label_),
          "PERSON"
        )
      },
      entities
    )

    if (length(entities) == 0L) {
      result[[i]] <- NULL
      next
    }

    result[[i]] <- data.frame(
      row_id = valid_indices[i],
      detected_name = vapply(
        entities,
        function(entity) {
          reticulate::py_to_r(entity$text)
        },
        character(1)
      ),
      start = vapply(
        entities,
        function(entity) {
          as.integer(reticulate::py_to_r(entity$start_char)) + 1L
        },
        integer(1)
      ),
      end = vapply(
        entities,
        function(entity) {
          as.integer(reticulate::py_to_r(entity$end_char))
        },
        integer(1)
      ),
      stringsAsFactors = FALSE
    )
  }

  result <- Filter(Negate(is.null), result)

  if (length(result) == 0L) {
    return(empty_result)
  }

  do.call(rbind, result)
}

mask_person_names_regex <- function(text,
                                    replacement = "[NAME]",
                                    apply_rules = TRUE) {
  output <- text

  for (row_id in seq_along(text)) {
    matches <- detect_person_names_regex(text[row_id])

    if (nrow(matches) == 0L || is.na(text[row_id])) {
      next
    }

    matches <- matches[
      order(matches$start, decreasing = TRUE),
      ,
      drop = FALSE
    ]

    current <- text[row_id]

    for (i in seq_len(nrow(matches))) {
      start <- matches$start[i]
      end <- matches$end[i]

      left <- if (start > 1L) substr(current, 1L, start - 1L) else ""
      right <- if (end < nchar(current)) substr(current, end + 1L, nchar(current)) else ""
      current <- paste0(left, replacement, right)
    }

    output[row_id] <- current
  }

  if (isTRUE(apply_rules)) {
    output <- apply_name_masking_rules(output, replacement = replacement)
  }

  output
}

detect_person_names_regex <- function(text) {
  empty_result <- data.frame(
    row_id = integer(),
    detected_name = character(),
    start = integer(),
    end = integer(),
    stringsAsFactors = FALSE
  )

  if (!is.character(text) || length(text) == 0L) {
    return(empty_result)
  }

  patterns <- c(
    paste0(
      "\\b",
      "(?:Mr|Mrs|Ms|Miss|Dr|Doctor|RN|Paramedic|EMT)",
      "\\.?\\s+",
      "(?:(?:[A-Z]\\.|[A-Z][A-Za-z'-]*)\\s*){1,3}"
    ),
    "\\b[A-Z][A-Za-z'-]+\\s+(?:[A-Z]\\.?\\s+)?[A-Z][A-Za-z'-]+\\b"
  )

  result <- vector("list", length(text))

  for (row_id in seq_along(text)) {
    value <- text[row_id]

    if (is.na(value) || !nzchar(trimws(value))) {
      result[[row_id]] <- NULL
      next
    }

    row_matches <- list()

    for (pattern in patterns) {
      matches <- gregexpr(pattern, value, perl = TRUE)[[1]]

      if (identical(matches[1], -1L)) {
        next
      }

      starts <- as.integer(matches)
      lengths <- attr(matches, "match.length")
      ends <- starts + lengths - 1L

      row_matches[[length(row_matches) + 1L]] <- data.frame(
        row_id = row_id,
        detected_name = mapply(
          function(start, end) {
            substr(value, start, end)
          },
          starts,
          ends,
          USE.NAMES = FALSE
        ),
        start = starts,
        end = ends,
        stringsAsFactors = FALSE
      )
    }

    if (length(row_matches) == 0L) {
      result[[row_id]] <- NULL
      next
    }

    row_result <- do.call(rbind, row_matches)
    row_result <- trim_detected_name_bounds(row_result)
    row_result <- remove_overlapping_name_matches(row_result)
    result[[row_id]] <- row_result
  }

  result <- Filter(Negate(is.null), result)

  if (length(result) == 0L) {
    return(empty_result)
  }

  out <- do.call(rbind, result)
  row.names(out) <- NULL
  out
}

trim_detected_name_bounds <- function(matches) {
  for (i in seq_len(nrow(matches))) {
    detected <- matches$detected_name[i]
    leading <- regexpr("\\S", detected, perl = TRUE)[1]

    if (!identical(leading, -1L) && leading > 1L) {
      matches$start[i] <- matches$start[i] + leading - 1L
    }

    trimmed <- trimws(detected)
    matches$end[i] <- matches$start[i] + nchar(trimmed) - 1L
    matches$detected_name[i] <- trimmed
  }

  matches
}

remove_overlapping_name_matches <- function(matches) {
  if (nrow(matches) <= 1L) {
    return(matches)
  }

  match_width <- matches$end - matches$start
  matches <- matches[
    order(matches$start, -match_width),
    ,
    drop = FALSE
  ]

  keep <- rep(FALSE, nrow(matches))
  kept_ranges <- data.frame(start = integer(), end = integer())

  for (i in seq_len(nrow(matches))) {
    overlaps <- nrow(kept_ranges) > 0L &&
      any(matches$start[i] <= kept_ranges$end & matches$end[i] >= kept_ranges$start)

    if (!isTRUE(overlaps)) {
      keep[i] <- TRUE
      kept_ranges <- rbind(
        kept_ranges,
        data.frame(start = matches$start[i], end = matches$end[i])
      )
    }
  }

  matches[keep, , drop = FALSE]
}

mask_single_spacy_document <- function(document, replacement) {
  original_text <- reticulate::py_to_r(document$text)

  entities <- reticulate::iterate(
    document$ents,
    simplify = FALSE
  )

  if (length(entities) == 0L) {
    return(original_text)
  }

  person_entities <- Filter(
    function(entity) {
      identical(
        reticulate::py_to_r(entity$label_),
        "PERSON"
      )
    },
    entities
  )

  if (length(person_entities) == 0L) {
    return(original_text)
  }

  positions <- data.frame(
    start = vapply(
      person_entities,
      function(entity) {
        as.integer(reticulate::py_to_r(entity$start_char))
      },
      integer(1)
    ),
    end = vapply(
      person_entities,
      function(entity) {
        as.integer(reticulate::py_to_r(entity$end_char))
      },
      integer(1)
    )
  )

  # Replace from right to left so earlier character offsets remain valid.
  positions <- positions[
    order(
      positions$start,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  output <- original_text

  for (i in seq_len(nrow(positions))) {
    # spaCy offsets are zero-based and end-exclusive.
    start_r <- positions$start[i] + 1L
    end_r <- positions$end[i]

    left_text <- if (start_r > 1L) {
      substr(
        output,
        1L,
        start_r - 1L
      )
    } else {
      ""
    }

    right_text <- if (end_r < nchar(output)) {
      substr(
        output,
        end_r + 1L,
        nchar(output)
      )
    } else {
      ""
    }

    output <- paste0(
      left_text,
      replacement,
      right_text
    )
  }

  output
}

validate_name_masking_inputs <- function(text,
                                         replacement,
                                         batch_size,
                                         keep_original) {
  if (!is.character(text)) {
    stop("`text` must be a character vector.", call. = FALSE)
  }

  if (
    !is.character(replacement) ||
      length(replacement) != 1L ||
      is.na(replacement)
  ) {
    stop("`replacement` must be one non-missing character value.", call. = FALSE)
  }

  if (
    length(batch_size) != 1L ||
      is.na(batch_size) ||
      batch_size < 1
  ) {
    stop("`batch_size` must be a positive integer.", call. = FALSE)
  }

  if (
    !is.logical(keep_original) ||
      length(keep_original) != 1L ||
      is.na(keep_original)
  ) {
    stop("`keep_original` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}
