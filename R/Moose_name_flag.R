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
#' Comma-separated names such as `Smith, John`, `SMITH, JOHN`, and
#' `St-Pierre, Anne-Marie` are detected. Canadian geographic forms such as
#' `Edmonton, Alberta` and `Avenue, Calgary` are excluded.
#' Healthcare organization names ending in words such as `Hospital`, `Clinic`,
#' `Centre`, `Health`, or `Foundation`, and clinical phrases containing words
#' such as `Chest`, `Pain`, `Disease`, or `Syndrome`, and treatment phrases such
#' as `Normal Saline IV`, and clinical document labels such as `RN Report`, are
#' excluded in both engines.
#' Non-person location phrases ending in `Residence`, `Store`, `Side`, or
#' `Airport`, and
#' clinical states containing `Syncopal`, `Syncope`, `Intoxicated`, or
#' `Intoxication`, are also excluded.
#' Common title-cased medical phrases, including `Chief Complaint`,
#' `Heat Exhaustion`, `Wellness Check`, `Safety Alerted`, `Not Feeling`,
#' `Bus Stop`, `Language Barrier`, `Situational Crisis`, and
#' `Non Small Cell Lung` are excluded through curated
#' non-person whitelists.
#' This whitelist takes precedence over the title-case name pattern, so audit
#' data where a real person's name could contain one of these terms.
#' The 334 official Alberta municipality names are also excluded in both
#' engines.
#' The Alberta place name `Fort McMurray` and the common spelling
#' `Fort McMurry` are also excluded.
#' The 1,150 unique facility names in the Alberta Health Services Find
#' Healthcare directory are excluded when the complete facility name occurs
#' in the text.
#' The 2,616 unique school names in the Alberta Education School Information
#' Report are also excluded when the complete school name occurs in the text.
#' The 12,435 English and French brand and active-ingredient names for marketed
#' or approved human drugs in the Health Canada Drug Product Database are also
#' excluded. Matching is case-insensitive and ignores accents and punctuation.
#' When `data` and `name_columns` are supplied, each text value is additionally
#' compared with the known names from the same row. These explicit references
#' take precedence over the non-person whitelists.
#'
#' @param text A character or factor vector, usually a column from a data frame.
#' @param batch_size Number of documents processed per spaCy batch.
#' @param engine Character. One of `"auto"`, `"spacy"`, or `"regex"`.
#' @param apply_rules Logical. If `TRUE`, include title-based and workflow-based
#'   supplementary rules, matching the default behavior of
#'   [Moose_mask_person_names()].
#' @param data Optional data frame containing row-aligned known-name columns.
#'   Supply this together with `name_columns`.
#' @param name_columns Optional character vector naming columns in `data`, in
#'   natural name order, such as `c("first_name", "last_name")`. Values are
#'   matched against `text` in the same row using case-insensitive whole-word
#'   matching. Full names and `Lastname, Firstname` forms are also matched.
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
#' records <- data.frame(
#'   comments = c("Alice Smith called.", "No name here."),
#'   first_name = c("Alice", "Bob"),
#'   last_name = c("Smith", "Jones")
#' )
#' Moose_name_flag(
#'   records$comments,
#'   engine = "regex",
#'   data = records,
#'   name_columns = c("first_name", "last_name")
#' )
#'
#' @export
Moose_name_flag <- function(text,
                            batch_size = 100L,
                            engine = c("auto", "spacy", "regex"),
                            apply_rules = TRUE,
                            data = NULL,
                            name_columns = NULL) {
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

  use_known_names <- validate_known_name_column_inputs(
    text = text,
    data = data,
    name_columns = name_columns
  )

  if (length(text) == 0L) {
    return(integer())
  }

  flag <- if (use_known_names) {
    flag_known_name_columns(
      text = text,
      data = data,
      name_columns = name_columns
    )
  } else {
    integer(length(text))
  }
  pending <- which(flag == 0L)

  if (!length(pending)) {
    return(flag)
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

  using_spacy <- identical(state$engine, "spacy") && !is.null(state$model)

  if (!using_spacy) {
    flag[pending] <- moose_name_flag_regex(text[pending])
  } else {
    flag[pending] <- moose_name_flag_spacy(
      text = text[pending],
      batch_size = batch_size,
      model = state$model
    )
  }

  if (isTRUE(apply_rules)) {
    flag[pending] <- pmax.int(
      flag[pending],
      moose_name_flag_supplementary(
        text[pending],
        include_title = using_spacy
      )
    )
  }

  flag
}

moose_name_flag_regex <- function(text) {
  moose_name_flag_patterns(text, name_person_regex_patterns())
}

moose_name_flag_patterns <- function(text, patterns) {
  flag <- integer(length(text))
  remaining <- which(!is.na(text) & nzchar(trimws(text)))

  if (!length(remaining)) {
    return(flag)
  }

  screened <- gsub(
    name_clinical_phrase_regex_pattern(),
    " | ",
    text[remaining],
    perl = TRUE
  )
  active <- seq_along(remaining)

  for (pattern in patterns) {
    pending <- active

    repeat {
      values <- screened[pending]
      matches <- regexpr(pattern, values, perl = TRUE)
      matched <- matches != -1L

      if (!any(matched)) {
        break
      }

      matched_pending <- pending[matched]
      starts <- as.integer(matches[matched])
      lengths <- attr(matches, "match.length")[matched]
      candidates <- substr(
        values[matched],
        starts,
        starts + lengths - 1L
      )
      geographic <-
        is_canadian_geographic_comma_candidate(candidates) |
        is_alberta_municipality_comma_candidate(candidates) |
        is_canadian_geographic_name_candidate(candidates)
      municipalities <- is_alberta_municipality_candidate(candidates)
      facilities <- is_alberta_health_facility_candidate_values(
        text = text[remaining[matched_pending]],
        candidate = candidates
      )
      schools <- is_alberta_school_candidate_values(
        text = text[remaining[matched_pending]],
        candidate = candidates
      )
      drugs <- is_health_canada_drug_candidate_values(
        text = text[remaining[matched_pending]],
        candidate = candidates
      )
      medical <- is_clinical_nonperson_candidate_values(
        text = values[matched],
        start = starts,
        end = starts + lengths - 1L,
        candidate = candidates
      )
      nonpeople <-
        geographic | municipalities | facilities | schools | drugs | medical
      people <- matched_pending[!nonpeople]

      if (length(people)) {
        flag[remaining[people]] <- 1L
      }

      nonperson_rows <- matched_pending[nonpeople]

      if (!length(nonperson_rows)) {
        break
      }

      nonperson_starts <- starts[nonpeople]
      nonperson_lengths <- lengths[nonpeople]
      nonperson_values <- screened[nonperson_rows]
      left <- ifelse(
        nonperson_starts > 1L,
        substr(nonperson_values, 1L, nonperson_starts - 1L),
        ""
      )
      right <- substr(
        nonperson_values,
        nonperson_starts + nonperson_lengths,
        nchar(nonperson_values)
      )
      screened[nonperson_rows] <- paste0(left, " | ", right)
      pending <- nonperson_rows
    }

    active <- active[flag[remaining[active]] == 0L]

    if (!length(active)) {
      break
    }
  }

  flag
}

moose_name_flag_supplementary <- function(text, include_title = TRUE) {
  patterns <- c(
    name_last_first_regex_pattern(),
    name_workflow_regex_pattern(exclude_municipalities = FALSE)
  )

  if (isTRUE(include_title)) {
    patterns <- c(
      name_title_regex_pattern(exclude_municipalities = FALSE),
      patterns
    )
  }

  moose_name_flag_patterns(text, patterns)
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
  document_text <- reticulate::py_to_r(document$text)
  labels <- reticulate::iterate(
    document$ents,
    f = function(entity) {
      spacy_entity_is_person(entity, document_text)
    },
    simplify = TRUE
  )

  any(labels)
}
