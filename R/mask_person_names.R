#' Detect and mask personal names in text
#'
#' Uses spaCy named-entity recognition when available. On locked-down
#' computers where Python cannot be initialized, \code{engine = "auto"} falls
#' back to a pure R regular-expression engine.
#'
#' In regex mode, uncontextualized all-uppercase phrases are not treated as
#' names. All-uppercase names are accepted after a supported title, such as
#' `RN` or `Dr.`, or after a supported workflow phrase when `apply_rules` is
#' `TRUE`.
#' Comma-separated names such as `Smith, John`, `SMITH, JOHN`, and
#' `St-Pierre, Anne-Marie` are detected in both engines. Canadian geographic
#' forms such as `Edmonton, Alberta` and `Avenue, Calgary` are excluded.
#' Healthcare organization names ending in words such as `Hospital`, `Clinic`,
#' `Centre`, `Health`, or `Foundation`, and clinical phrases containing words
#' such as `Chest`, `Pain`, `Disease`, or `Syndrome`, and treatment phrases such
#' as `Normal Saline IV`, and clinical document labels such as `RN Report`, are
#' excluded from name matching in both engines.
#' Non-person location phrases ending in `Residence`, `Store`, `Side`, or
#' `Airport`, and
#' clinical states containing `Syncopal`, `Syncope`, `Intoxicated`, or
#' `Intoxication`, are also excluded.
#' Common title-cased medical phrases, including `Chief Complaint`,
#' `Heat Exhaustion`, `Wellness Check`, `Safety Alerted`, `Not Feeling`,
#' `Bus Stop`, `Language Barrier`, `Cognitive Impairment`, `Emerge Call`,
#' `Contact Droplet`, `Patient Name`,
#' `Situational Crisis`, and
#' `Non Small Cell Lung` are excluded through curated
#' non-person whitelists.
#' This whitelist takes precedence over the title-case name pattern, so audit
#' data where a real person's name could contain one of these terms.
#' The 334 official Alberta municipality names are also excluded in both
#' engines, including names such as `Medicine Hat`, `Red Deer`, and
#' `Rocky Mountain House`.
#' The Alberta place name `Fort McMurray` and the common spelling
#' `Fort McMurry` are also excluded.
#' The 1,150 unique facility names in the Alberta Health Services Find
#' Healthcare directory are excluded when the complete facility name occurs
#' in the text, including `Peter Lougheed Centre`, `Grand Manor`, and
#' `Chartwell Griesbach`.
#' The 2,616 unique school names in the Alberta Education School Information
#' Report are also excluded when the complete school name occurs in the text.
#' The 12,435 English and French brand and active-ingredient names for marketed
#' or approved human drugs in the Health Canada Drug Product Database are also
#' excluded. Matching is case-insensitive and ignores accents and punctuation.
#' When `data` and `name_columns` are supplied, each text value is additionally
#' compared with the known names from the same row. These explicit references
#' take precedence over the non-person whitelists.
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
#' @param data Optional data frame containing row-aligned known-name columns.
#'   Supply this together with `name_columns`.
#' @param name_columns Optional character vector naming columns in `data`, in
#'   natural name order, such as `c("first_name", "last_name")`. Values are
#'   matched against `text` in the same row using case-insensitive whole-word
#'   matching. Full names and `Lastname, Firstname` forms are also matched.
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
                              apply_rules = TRUE,
                              data = NULL,
                              name_columns = NULL) {
  validate_name_masking_inputs(text, replacement, batch_size, keep_original)
  engine <- match.arg(engine)

  if (!is.logical(apply_rules) || length(apply_rules) != 1L || is.na(apply_rules)) {
    stop("`apply_rules` must be TRUE or FALSE.", call. = FALSE)
  }

  use_known_names <- validate_known_name_column_inputs(
    text = text,
    data = data,
    name_columns = name_columns
  )

  if (length(text) == 0L) {
    return(text)
  }

  input_text <- if (use_known_names) {
    mask_known_name_columns(
      text = text,
      replacement = replacement,
      data = data,
      name_columns = name_columns
    )
  } else {
    text
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
      text = input_text,
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

  missing_input <- is.na(input_text)
  blank_input <- !missing_input & !nzchar(trimws(input_text))
  process_input <- !missing_input & !blank_input

  output <- input_text

  if (any(process_input)) {
    process_text <- input_text[process_input]

    python_text <- reticulate::r_to_py(
      as.list(process_text),
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
#' Detects conventional and comma-separated personal names, including
#' `Lastname, Firstname` forms.
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
    document_text <- reticulate::py_to_r(document_list[[i]]$text)
    entities <- reticulate::iterate(
      document_list[[i]]$ents,
      simplify = FALSE
    )

    bounds <- lapply(
      entities,
      spacy_person_entity_bounds,
      document_text = document_text
    )
    bounds <- Filter(Negate(is.null), bounds)

    if (length(bounds) == 0L) {
      result[[i]] <- NULL
      next
    }

    result[[i]] <- data.frame(
      row_id = valid_indices[i],
      detected_name = vapply(
        bounds,
        function(bound) {
          substr(document_text, bound$start, bound$end)
        },
        character(1)
      ),
      start = vapply(
        bounds,
        function(bound) {
          bound$start
        },
        integer(1)
      ),
      end = vapply(
        bounds,
        function(bound) {
          bound$end
        },
        integer(1)
      ),
      stringsAsFactors = FALSE
    )
  }

  result <- Filter(Negate(is.null), result)
  spacy_result <- if (length(result)) do.call(rbind, result) else empty_result
  comma_result <- detect_person_names_regex(text)
  comma_result <- comma_result[
    grepl(",", comma_result$detected_name, fixed = TRUE),
    ,
    drop = FALSE
  ]
  combined <- rbind(spacy_result, comma_result)

  if (nrow(combined) == 0L) {
    return(empty_result)
  }

  combined <- do.call(
    rbind,
    lapply(
      split(combined, combined$row_id),
      remove_overlapping_name_matches
    )
  )
  combined <- combined[order(combined$row_id, combined$start), , drop = FALSE]
  row.names(combined) <- NULL
  combined
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

  patterns <- name_person_regex_patterns()

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
    keep <- !mapply(
      is_nonperson_name_candidate,
      text = value,
      start = row_result$start,
      end = row_result$end,
      USE.NAMES = FALSE
    )
    row_result <- row_result[keep, , drop = FALSE]

    if (nrow(row_result) == 0L) {
      result[[row_id]] <- NULL
      next
    }

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

name_person_regex_patterns <- function() {
  person_word <- name_person_title_case_word_pattern()

  c(
    name_last_first_regex_pattern(),
    name_patient_abbreviation_regex_pattern(),
    name_title_regex_pattern(exclude_municipalities = FALSE),
    paste0(
      "\\b",
      person_word,
      "\\s+(?:[A-Z]\\.\\s+)?",
      person_word,
      "(?![A-Za-z'-])",
      name_nonperson_suffix_guard()
    )
  )
}

name_last_first_regex_pattern <- function() {
  word <- name_context_word_pattern()

  paste0(
    "\\b",
    word,
    "\\s*,\\s*",
    word,
    "(?:\\s+", word, "){0,2}",
    "(?![A-Za-z'-])",
    name_nonperson_suffix_guard()
  )
}

name_patient_abbreviation_regex_pattern <- function() {
  word <- name_context_word_pattern()

  paste0(
    "(?i:\\bPt\\.?\\s+)",
    "\\K",
    word,
    "(?:\\s+", word, "){0,2}",
    "(?![A-Za-z'-])",
    name_nonperson_suffix_guard()
  )
}

name_title_case_word_pattern <- function() {
  paste0(
    "(?:",
    "[A-Z][a-z]+(?:[A-Z][a-z]+)*(?:[-'][A-Z]?[a-z]+)*",
    "|[A-Z](?:['-][A-Z]?[a-z]+)+",
    ")"
  )
}

name_context_word_pattern <- function() {
  paste0(
    "(?!", name_regex_excluded_word_pattern(), "\\b)",
    "(?:",
    "[A-Z]\\.",
    "|", name_title_case_word_pattern(),
    "|[A-Z]{2,}(?:[-'][A-Z]+)*",
    "|[A-Z](?:['-][A-Z]+)+",
    ")"
  )
}

name_person_title_case_word_pattern <- function() {
  paste0(
    "(?!(?i:Pt)\\b)",
    "(?!", name_regex_excluded_word_pattern(), "\\b)",
    name_title_case_word_pattern()
  )
}

name_organization_word_pattern <- function() {
  paste0(
    "(?i:(?:",
    paste(
      c(
        "Hospital", "Hospitals", "Clinic", "Clinics", "Centre", "Centres",
        "Center", "Centers", "Health", "Healthcare", "Medical", "Care",
        "Authority", "Hospice", "Foundation", "City", "Town", "Village",
        "Municipal", "Municipality", "County", "District", "Improvement",
        "Special", "Areas", "School", "Schools", "Academy", "Academies",
        "College", "Collegiate", "Campus", "Institute"
      ),
      collapse = "|"
    ),
    "))"
  )
}

name_location_word_pattern <- function() {
  "(?i:(?:Residence|Residences|Store|Stores|Side|Airport|Airports))"
}

name_canadian_geographic_suffix_pattern <- function() {
  paste0(
    "(?i:(?:",
    paste(
      c(
        "Alberta", "British Columbia", "Saskatchewan", "Manitoba",
        "Ontario", "Quebec", "New Brunswick", "Nova Scotia",
        "Prince Edward Island", "Newfoundland and Labrador", "Yukon",
        "Northwest Territories", "Nunavut", "Canada",
        "AB", "BC", "SK", "MB", "ON", "QC", "NB", "NS", "PE",
        "NL", "YT", "NT", "NU"
      ),
      collapse = "|"
    ),
    "))"
  )
}

is_canadian_geographic_comma_candidate <- function(candidate) {
  grepl(
    paste0(",\\s*", name_canadian_geographic_suffix_pattern(), "\\.?$"),
    candidate,
    perl = TRUE
  )
}

is_alberta_municipality_comma_candidate <- function(candidate) {
  has_comma <- !is.na(candidate) & grepl(",", candidate, fixed = TRUE)
  result <- rep.int(FALSE, length(candidate))

  if (!any(has_comma)) {
    return(result)
  }

  suffix <- sub("^.*,\\s*", "", candidate[has_comma], perl = TRUE)
  suffix <- gsub(
    "^[[:punct:][:space:]]+|[[:punct:][:space:]]+$",
    "",
    suffix,
    perl = TRUE
  )
  result[has_comma] <- is_alberta_municipality_name(suffix)
  result
}

is_canadian_geographic_name_candidate <- function(candidate) {
  grepl(
    paste0("^", name_canadian_geographic_suffix_pattern(), "\\.?$"),
    trimws(candidate),
    perl = TRUE
  )
}

name_medical_abbreviations <- function() {
  c(
    # Clinical history, assessment, and documentation
    "Hx", "PMH", "PMHx", "PSH", "PSHx", "FHx", "SHx", "HPI", "RS",
    "ROS", "PE", "Dx", "Rx", "Tx", "Px", "Sx", "AD", "DC", "TR",
    # Care settings and status
    "ER", "IP", "CC", "OP", "CN", "RH", "PR", "MU", "NKA", "NKDA",
    "NAD", "WNL", "LOC", "SOB",
    # Vitals and common EMS event shorthand
    "BP", "HR", "RR", "SpO2", "GCS", "BGL", "ETOH", "MVC", "MVA", "GSW",
    "CVA", "TIA", "MI", "CHF", "COPD", "CAD", "UTI", "HTN", "DVT",
    "AFib", "IFT"
  )
}

name_clinical_terms <- function() {
  c(
    name_medical_abbreviations(),
    # Clinical context and documentation
    "Clinical", "Emergency", "Diagnosis", "Treatment", "Therapy", "Surgery",
    "Age", "Aged", "Year", "Years", "Old",
    "Procedure", "Complain", "Complaint", "Complaints", "Present", "Past",
    "History", "Histories", "Illness", "Illnesses", "Transplant",
    "Transplants", "Transplantation", "Nursing", "Nurse", "Nurses",
    "Practitioner", "Practitioners",
    "Assessment", "Documentation", "Report", "Reports", "Note", "Notes",
    "Event", "Events",
    "Cause", "Causes", "Caused", "Causation",
    "Record", "Records", "Chart", "Charts", "Form", "Forms", "Summary",
    "Summaries", "Demographic", "Demographics", "Handover", "Triage",
    "Transport", "Transfer",
    "Interfacility", "Intrafacility", "Interhospital", "Interagency",
    "Arrival", "Arrivals", "Arrived", "Scene", "Scenes", "Admission", "Admissions",
    "Admitted", "Discharge", "Discharges", "Discharged", "Presentation",
    "Presentations", "Presented", "Enroute", "Route", "Routes", "Routing",
    "Departure", "Departed",
    "Ambulance", "Community", "Communities", "Assist", "Assists", "Assistance", "Resuscitation",
    "Extrication", "Status", "Activity",
    "Code", "Codes",
    # Severity, anatomy, and body systems
    "Acute", "Chronic", "Severe", "Cardia", "Cardiac", "Cardiovascular",
    "Chest", "Abdomen", "Abdominal", "Respiratory", "Renal", "Airway",
    "Head", "Neck", "Back", "Shoulder", "Arm", "Elbow", "Wrist", "Hand",
    "Hip", "Leg", "Knee", "Ankle", "Foot", "Facial", "Cervical", "Lumbar",
    "Thoracic", "Cranial", "Spinal", "Spine", "Pelvic", "Pelvis",
    "Pulmonary", "Neurological", "Neuro", "Gastric", "Gastrointestinal",
    "Urinary", "Vascular", "Arterial", "Venous", "Left", "Right",
    # Symptoms, conditions, and injuries
    "Trauma", "Pain", "Disease", "Syndrome", "Disorder", "Injury",
    "Failure", "Fracture", "Cancer", "Infection", "Covid", "Coronavirus",
    "Screen", "Screens", "Screened", "Screening", "Symptom", "Symptoms",
    "Issue", "Issues",
    "Altered", "Mental", "Consciousness", "Alert", "Oriented", "Confused",
    "Confusion", "Unconscious", "Unresponsive", "Lethargic", "Agitated",
    "Combative", "Delirium", "Psychiatric", "Psychosis", "Anxiety",
    "Depression", "Behavior", "Behaviour", "Behavioral", "Behavioural",
    "Problem", "Problems", "Suicidal", "Homicidal", "Ideation", "Dyspnea", "Apnea",
    "Wheezing", "Cough", "Hypoxia", "Hypoxic", "Nausea", "Vomiting",
    "Diarrhea", "Constipation", "Dysphagia", "Dizziness", "Dizzy", "Vertigo",
    "Headache", "Migraine", "Fever", "Chills", "Fatigue", "Weakness",
    "Numbness", "Tingling", "Paralysis", "Seizure", "Seizures", "Stroke",
    "Animal", "Dog", "Dogs", "Canine", "Bite", "Bites", "Bitten",
    "Nose", "Nasal", "Bleed", "Bleeds", "Bleeding", "Hemorrhage",
    "Laceration", "Lacerations", "Abrasion",
    "Abrasions", "Contusion", "Contusions", "Dislocation", "Dislocations",
    "Sprain", "Sprains", "Strain", "Strains", "Wound", "Wounds", "Edema",
    "Swelling", "Tenderness", "Rash", "Retention", "Dehydration",
    "Hypoglycemia", "Hyperglycemia", "Diabetic", "Tachycardia",
    "Bradycardia", "Arrhythmia", "Palpitations", "Hypotension",
    "Hypertension", "Sepsis", "Septic", "Pneumonia", "Allergic",
    "Anaphylaxis", "Anaphylactic", "Reaction", "Overdose", "Opioid",
    "Alcohol", "Withdrawal", "Poisoning", "Substance", "Abuse", "Misuse",
    "Dependence", "Dependency", "Addiction", "Addicted", "Syncopal",
    "Syncope", "Intoxicated", "Intoxication",
    # Vitals, events, equipment, and treatments
    "Blood", "Pressure", "Heart", "Pulse", "Rate", "Rhythm", "Oxygen",
    "Saturation", "Temperature", "Glucose", "Systolic", "Diastolic",
    "Motor", "Vehicle", "Collision", "Truck", "Trucks", "Ground", "Level", "Mechanical",
    "Witnessed", "Unwitnessed", "Normal", "Saline", "IV", "Intravenous",
    "Infusion", "Bolus", "Dose", "Medication", "Solution", "Ventilation",
    "Ventilator", "Nebulizer", "Catheter", "Cannula", "Tourniquet", "Splint",
    "Bandage", "Dressing", "Defibrillator", "Defibrillation", "CPR", "ECG",
    "EKG", "Aspirin", "Epinephrine", "Adrenaline", "Naloxone", "Narcan",
    "Nitroglycerin", "Insulin", "Dextrose", "Morphine", "Fentanyl",
    "Ketamine", "Acetaminophen", "Tylenol", "Ibuprofen", "Advil",
    "Albuterol", "Salbutamol", "Atrovent"
  )
}

name_clinical_phrases <- function() {
  c(
    "Heat Exhaustion",
    "Dispatched",
    "General Malaise",
    "Supportive Living",
    "Wellness Check",
    "Refill Prescription",
    "Return Trip",
    "Edmonton General",
    "Safety Alerted",
    "Not Feeling",
    "Bus Stop",
    "Language Barrier",
    "Cognitive Impairment",
    "Emerge Call",
    "Contact Droplet",
    "Patient Name",
    "Non Small Cell Lung",
    "Non-Small Cell Lung",
    "Situational Crisis"
  )
}

normalize_clinical_phrase <- function(value) {
  value <- trimws(as.character(value))
  value <- gsub("[[:space:]]+", " ", value, perl = TRUE)
  value <- gsub(
    "^[[:punct:][:space:]]+|[[:punct:][:space:]]+$",
    "",
    value,
    perl = TRUE
  )
  tolower(value)
}

is_clinical_phrase_candidate_values <- function(candidate) {
  normalize_clinical_phrase(candidate) %in%
    normalize_clinical_phrase(name_clinical_phrases())
}

name_clinical_phrase_regex_pattern <- local({
  pattern <- NULL

  function() {
    if (!is.null(pattern)) {
      return(pattern)
    }

    phrases <- name_clinical_phrases()
    phrases <- phrases[order(nchar(phrases), decreasing = TRUE)]
    escaped <- escape_name_regex_literal(phrases)
    escaped <- gsub(" ", "\\s+", escaped, fixed = TRUE)
    pattern <<- paste0(
      "(?<![A-Za-z'-])(?i:(?:",
      paste(escaped, collapse = "|"),
      "))(?![A-Za-z'-])"
    )
    pattern
  }
})

is_clinical_phrase_span_values <- function(text, start, end) {
  lengths <- c(length(text), length(start), length(end))

  if (length(unique(lengths)) != 1L) {
    stop("Clinical phrase span inputs must have the same length.", call. = FALSE)
  }

  result <- rep.int(FALSE, length(text))
  valid <- which(!is.na(text) & !is.na(start) & !is.na(end))

  for (i in valid) {
    matches <- gregexpr(
      name_clinical_phrase_regex_pattern(),
      text[i],
      perl = TRUE
    )[[1]]

    if (identical(matches[1], -1L)) {
      next
    }

    match_ends <- matches + attr(matches, "match.length") - 1L
    result[i] <- any(matches <= start[i] & match_ends >= end[i])
  }

  result
}

name_clinical_word_pattern <- local({
  pattern <- paste0(
    "(?i:(?:",
    paste(unique(name_clinical_terms()), collapse = "|"),
    "))"
  )

  function() pattern
})

name_regex_excluded_word_pattern <- function() {
  paste0(
    "(?:",
    name_organization_word_pattern(),
    "|",
    name_location_word_pattern(),
    ")"
  )
}

name_nonperson_word_pattern <- function() {
  paste0(
    "(?:",
    name_regex_excluded_word_pattern(),
    "|",
    name_clinical_word_pattern(),
    ")"
  )
}

name_nonperson_suffix_guard <- function() {
  context_word <- name_context_word_pattern()

  paste0(
    "(?!\\s+(?:",
    context_word,
    "\\s+){0,2}",
    name_regex_excluded_word_pattern(),
    "\\b)"
  )
}

is_clinical_nonperson_candidate_values <- function(text, start, end, candidate) {
  lengths <- c(length(text), length(start), length(end), length(candidate))

  if (length(unique(lengths)) != 1L) {
    stop("Clinical candidate inputs must have the same length.", call. = FALSE)
  }

  result <-
    is_clinical_phrase_candidate_values(candidate) |
    is_clinical_phrase_span_values(text, start, end)

  if (!length(candidate)) {
    return(result)
  }

  candidate_keys <- ifelse(is.na(candidate), "", candidate)
  pending_candidates <- which(!result)

  if (!length(pending_candidates)) {
    return(result)
  }

  pending_keys <- candidate_keys[pending_candidates]
  unique_candidates <- !duplicated(pending_keys)
  unique_result <- grepl(
    paste0("\\b", name_clinical_word_pattern(), "\\b"),
    pending_keys[unique_candidates],
    perl = TRUE
  )
  result[pending_candidates] <- unique_result[
    match(pending_keys, pending_keys[unique_candidates])
  ]
  pending <- which(!result & !is.na(text) & end < nchar(text))

  if (!length(pending)) {
    return(result)
  }

  suffix <- substr(text[pending], end[pending] + 1L, nchar(text[pending]))
  unique_suffixes <- !duplicated(suffix)
  suffix_result <- grepl(
    paste0(
      "^\\s+(?:",
      name_context_word_pattern(),
      "\\s+){0,2}",
      name_clinical_word_pattern(),
      "\\b"
    ),
    suffix[unique_suffixes],
    perl = TRUE
  )
  result[pending] <- suffix_result[match(suffix, suffix[unique_suffixes])]
  result
}

is_nonperson_name_candidate <- function(text, start, end) {
  candidate <- substr(text, start, end)

  if (
    is_clinical_phrase_candidate_values(candidate) ||
      is_clinical_phrase_span_values(text, start, end)
  ) {
    return(TRUE)
  }

  if (
    is_canadian_geographic_comma_candidate(candidate) ||
      is_alberta_municipality_comma_candidate(candidate) ||
      is_canadian_geographic_name_candidate(candidate)
  ) {
    return(TRUE)
  }

  if (is_alberta_municipality_candidate(candidate)) {
    return(TRUE)
  }

  if (is_alberta_health_facility_candidate(text, start, end)) {
    return(TRUE)
  }

  if (is_alberta_school_candidate(text, start, end)) {
    return(TRUE)
  }

  if (is_health_canada_drug_candidate(text, start, end)) {
    return(TRUE)
  }

  nonperson_word <- name_nonperson_word_pattern()

  if (grepl(paste0("\\b", nonperson_word, "\\b"), candidate, perl = TRUE)) {
    return(TRUE)
  }

  if (end >= nchar(text)) {
    return(FALSE)
  }

  suffix <- substr(text, end + 1L, nchar(text))

  grepl(
    paste0(
      "^\\s+(?:",
      name_context_word_pattern(),
      "\\s+){0,2}",
      nonperson_word,
      "\\b"
    ),
    suffix,
    perl = TRUE
  )
}

patient_abbreviation_adjusted_bounds <- function(text, start, end) {
  candidate <- substr(text, start, end)

  if (grepl("(?i)^Pt\\.?$", trimws(candidate), perl = TRUE)) {
    return(NULL)
  }

  prefix <- regexpr("(?i)^Pt\\.?\\s+", candidate, perl = TRUE)

  if (identical(as.integer(prefix), 1L)) {
    start <- start + as.integer(attr(prefix, "match.length"))
  }

  list(start = as.integer(start), end = as.integer(end))
}

spacy_person_entity_bounds <- function(entity, document_text) {
  if (!identical(reticulate::py_to_r(entity$label_), "PERSON")) {
    return(NULL)
  }

  start <- as.integer(reticulate::py_to_r(entity$start_char)) + 1L
  end <- as.integer(reticulate::py_to_r(entity$end_char))
  bounds <- patient_abbreviation_adjusted_bounds(document_text, start, end)

  if (is.null(bounds)) {
    return(NULL)
  }

  if (is_nonperson_name_candidate(document_text, bounds$start, bounds$end)) {
    return(NULL)
  }

  bounds
}

spacy_entity_is_person <- function(entity, document_text) {
  !is.null(spacy_person_entity_bounds(entity, document_text))
}

name_title_regex_pattern <- function(exclude_municipalities = TRUE) {
  word <- name_context_word_pattern()
  municipality_guard <- if (isTRUE(exclude_municipalities)) {
    alberta_municipality_exact_guard()
  } else {
    ""
  }

  paste0(
    "\\b(?:Mr|Mrs|Ms|Miss|Dr|Doctor|RN|Paramedic|EMT)",
    "\\.?\\s+",
    municipality_guard,
    word,
    "(?:\\s+", word, "){0,2}",
    "(?![A-Za-z'-])",
    name_nonperson_suffix_guard()
  )
}

name_workflow_regex_pattern <- function(exclude_municipalities = TRUE) {
  word <- name_context_word_pattern()
  municipality_guard <- if (isTRUE(exclude_municipalities)) {
    alberta_municipality_exact_guard()
  } else {
    ""
  }

  paste0(
    "(?i:\\b(?:reviewed|assessed|signed|completed|reported)\\s+by\\s+)",
    "\\K",
    municipality_guard,
    word,
    "(?:\\s+", word, "){0,2}",
    "(?![A-Za-z'-])",
    name_nonperson_suffix_guard()
  )
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

  person_bounds <- lapply(
    entities,
    spacy_person_entity_bounds,
    document_text = original_text
  )
  person_bounds <- Filter(Negate(is.null), person_bounds)

  if (length(person_bounds) == 0L) {
    return(original_text)
  }

  positions <- data.frame(
    start = vapply(
      person_bounds,
      function(bound) {
        bound$start
      },
      integer(1)
    ),
    end = vapply(
      person_bounds,
      function(bound) {
        bound$end
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
    start_r <- positions$start[i]
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
