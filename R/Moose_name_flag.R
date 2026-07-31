#' Flag text values containing a detected personal name
#'
#' Uses the same spaCy or pure R regex engine as
#' [Moose_detect_person_names()] to inspect a character or factor vector.
#' The returned integer vector can be assigned directly inside
#' `dplyr::mutate()`. `1L` means at least one personal name was detected in the
#' corresponding value, and `0L` means no name was detected. Missing and blank
#' values receive `0L`.
#'
#' For large data sets, call this function on an ungrouped data frame so
#' `dplyr::mutate()` evaluates the complete column once rather than once per
#' group.
#' In regex mode, standalone all-uppercase phrases are ignored. All-uppercase
#' names are accepted only after a supported title or, when `apply_rules` is
#' `TRUE`, a supported workflow phrase such as `Reviewed by`.
#' Healthcare organization names ending in words such as `Hospital`, `Clinic`,
#' `Centre`, `Health`, or `Foundation`, and clinical phrases containing words
#' such as `Chest`, `Pain`, `Disease`, or `Syndrome`, are excluded.
#'
#' @param text A character or factor vector, usually a column from a data frame.
#' @param batch_size Number of documents processed per spaCy batch.
#' @param engine Character. One of `"auto"`, `"spacy"`, or `"regex"`.
#' @param apply_rules Logical. If `TRUE`, include title-based and workflow-based
#'   supplementary rules, matching the default behavior of
#'   [Moose_mask_person_names()].
#'
#' @return An integer vector with the same length as `text`, containing `1L`
#'   when a personal name is detected and `0L` otherwise.
#'
#' @examples
#' comments <- c(
#'   "John Smith reviewed the file.",
#'   "nothing to report",
#'   NA
#' )
#' Moose_name_flag(comments, engine = "regex")
#'
#' @export
Moose_name_flag <- function(text,
                            batch_size = 100L,
                            engine = c("auto", "spacy", "regex"),
                            apply_rules = TRUE) {
  engine <- match.arg(engine)

  if (is.factor(text)) {
    text <- as.character(text)
  }

  if (!is.character(text)) {
    stop("`text` must be a character or factor vector.", call. = FALSE)
  }

  validate_name_masking_inputs(
    text = text,
    replacement = "[NAME]",
    batch_size = batch_size,
    keep_original = FALSE
  )

  if (!is.logical(apply_rules) ||
      length(apply_rules) != 1L ||
      is.na(apply_rules)) {
    stop("`apply_rules` must be TRUE or FALSE.", call. = FALSE)
  }

  if (length(text) == 0L) {
    return(integer())
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
    flag <- moose_name_flag_regex(text)
  } else {
    flag <- moose_name_flag_spacy(
      text = text,
      batch_size = batch_size,
      model = state$model
    )
  }

  if (isTRUE(apply_rules)) {
    flag <- pmax.int(flag, moose_name_flag_supplementary(text))
  }

  flag
}

moose_name_flag_regex <- function(text) {
  flag <- integer(length(text))
  remaining <- which(!is.na(text) & nzchar(trimws(text)))

  if (!length(remaining)) {
    return(flag)
  }

  for (pattern in name_person_regex_patterns()) {
    matched <- grepl(pattern, text[remaining], perl = TRUE)

    if (any(matched)) {
      flag[remaining[matched]] <- 1L
      remaining <- remaining[!matched]
    }

    if (!length(remaining)) {
      break
    }
  }

  flag
}

moose_name_flag_supplementary <- function(text) {
  flag <- integer(length(text))
  valid <- which(!is.na(text) & nzchar(trimws(text)))

  if (!length(valid)) {
    return(flag)
  }

  patterns <- c(
    name_title_regex_pattern(),
    name_workflow_regex_pattern()
  )

  for (pattern in patterns) {
    flag[valid[grepl(pattern, text[valid], perl = TRUE)]] <- 1L
  }

  flag
}

moose_name_flag_spacy <- function(text, batch_size, model) {
  flag <- integer(length(text))
  valid_indices <- which(!is.na(text) & nzchar(trimws(text)))

  if (!length(valid_indices)) {
    return(flag)
  }

  batch_size <- as.integer(batch_size)
  chunk_size <- as.integer(max(
    10000,
    min(100000, as.double(batch_size) * 25)
  ))

  chunk_starts <- seq.int(
    from = 1L,
    to = length(valid_indices),
    by = chunk_size
  )

  for (chunk_start in chunk_starts) {
    chunk_end <- min(chunk_start + chunk_size - 1L, length(valid_indices))
    chunk_rows <- valid_indices[seq.int(chunk_start, chunk_end)]

    documents <- model$pipe(
      reticulate::r_to_py(
        as.list(text[chunk_rows]),
        convert = FALSE
      ),
      batch_size = batch_size
    )

    chunk_flag <- reticulate::iterate(
      documents,
      f = moose_spacy_document_has_person,
      simplify = TRUE
    )
    chunk_flag <- as.logical(chunk_flag)

    if (length(chunk_flag) != length(chunk_rows)) {
      stop(
        "spaCy returned an unexpected number of documents.",
        call. = FALSE
      )
    }

    flag[chunk_rows] <- as.integer(chunk_flag)
  }

  flag
}

moose_spacy_document_has_person <- function(document) {
  labels <- reticulate::iterate(
    document$ents,
    f = function(entity) {
      identical(reticulate::py_to_r(entity$label_), "PERSON")
    },
    simplify = TRUE
  )

  any(labels)
}
