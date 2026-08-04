library(MooseR)

facilities <- MooseR:::alberta_health_facilities()

stopifnot(
  length(facilities) == 1150L,
  !anyDuplicated(facilities)
)

schools <- MooseR:::alberta_schools()

stopifnot(
  length(schools) == 2616L,
  !anyDuplicated(tolower(schools)),
  all(MooseR:::is_alberta_school_candidate_values(schools, schools)),
  all(MooseR:::is_alberta_school_candidate_values(
    toupper(schools),
    toupper(schools)
  ))
)

drugs <- MooseR:::health_canada_drugs()
normalized_drugs <- MooseR:::health_canada_drug_names_normalized()

stopifnot(
  length(drugs) == 12435L,
  length(normalized_drugs) == 11990L,
  !anyDuplicated(normalized_drugs),
  all(MooseR:::is_health_canada_drug_candidate_values(drugs, drugs)),
  all(MooseR:::is_health_canada_drug_candidate_values(
    toupper(drugs),
    toupper(drugs)
  ))
)

drug_examples <- c(
  "Abilify Maintena",
  "Abiraterone Acetate",
  "Acamprosate Calcique",
  "Acétate d'abiratérone",
  "Teva Quinine"
)

stopifnot(
  all(MooseR:::is_health_canada_drug_candidate_values(
    drug_examples,
    drug_examples
  )),
  MooseR:::is_health_canada_drug_candidate_value(
    "Abilify Maintena was administered.",
    "Maintena"
  ),
  !MooseR:::is_health_canada_drug_candidate_value(
    "Maintena was documented.",
    "Maintena"
  ),
  !MooseR:::is_health_canada_drug_candidate_value(
    "John Smith was documented.",
    "John Smith"
  )
)

drug_context <- c(
  "Abilify Maintena was administered.",
  "Reviewed by Abilify Maintena.",
  "John Smith received Abilify Maintena."
)

stopifnot(
  identical(
    Moose_mask_person_names(drug_context, engine = "regex"),
    c(
      "Abilify Maintena was administered.",
      "Reviewed by Abilify Maintena.",
      "[NAME] received Abilify Maintena."
    )
  ),
  identical(
    Moose_name_flag(drug_context, engine = "regex"),
    c(0L, 0L, 1L)
  ),
  identical(
    Moose_detect_person_names(drug_context, engine = "regex")$detected_name,
    "John Smith"
  )
)

facility_examples <- c(
  "Peter Lougheed Centre",
  "Grand Manor",
  "Chartwell Griesbach",
  "Alberta Children's Hospital",
  "Sexsmith Medical Clinic"
)

stopifnot(
  identical(
    Moose_mask_person_names(facility_examples, engine = "regex"),
    facility_examples
  ),
  identical(
    Moose_name_flag(facility_examples, engine = "regex"),
    integer(length(facility_examples))
  ),
  nrow(Moose_detect_person_names(facility_examples, engine = "regex")) == 0L
)

school_examples <- c(
  "Prairiehome Colony School",
  "FFCA High School Campus",
  "The Chinese Academy",
  "Centre High",
  "The Academy at King Edward",
  "Meskanahk Ka-Nipa-Wit School"
)

stopifnot(
  all(school_examples %in% schools),
  identical(
    Moose_mask_person_names(school_examples, engine = "regex"),
    school_examples
  ),
  identical(
    Moose_name_flag(school_examples, engine = "regex"),
    integer(length(school_examples))
  ),
  nrow(Moose_detect_person_names(school_examples, engine = "regex")) == 0L
)

school_context <- c(
  "John Smith visited The Academy at King Edward.",
  "Reviewed by The Chinese Academy.",
  "King Edward met Sarah Johnson."
)

stopifnot(
  identical(
    Moose_mask_person_names(school_context, engine = "regex"),
    c(
      "[NAME] visited The Academy at King Edward.",
      "Reviewed by The Chinese Academy.",
      "[NAME] met [NAME]."
    )
  ),
  identical(
    Moose_name_flag(school_context, engine = "regex"),
    c(1L, 0L, 1L)
  )
)

mixed <- c(
  "John Smith visited Peter Lougheed Centre.",
  "Reviewed by Grand Manor.",
  "Peter Lougheed met Sarah Johnson."
)

stopifnot(
  identical(
    Moose_mask_person_names(mixed, engine = "regex"),
    c(
      "[NAME] visited Peter Lougheed Centre.",
      "Reviewed by Grand Manor.",
      "[NAME] met [NAME]."
    )
  ),
  identical(
    Moose_name_flag(mixed, engine = "regex"),
    c(1L, 0L, 1L)
  )
)

nonperson_examples <- c(
  "Private Residence",
  "West Side",
  "Costco Store",
  "Pt Syncopal",
  "Heavily Intoxicated",
  "Fort McMurray",
  "Fort McMurry",
  "Reviewed by Fort McMurry.",
  "Edmonton Airport",
  "Edmonton International Airport",
  "Reviewed by Edmonton Airport."
)

stopifnot(
  all(MooseR:::is_alberta_municipality_name(c(
    "Fort McMurray",
    "FORT MCMURRAY",
    "fort mcmurry"
  ))),
  identical(
    Moose_mask_person_names(nonperson_examples, engine = "regex"),
    nonperson_examples
  ),
  identical(
    Moose_name_flag(nonperson_examples, engine = "regex"),
    integer(length(nonperson_examples))
  ),
  nrow(Moose_detect_person_names(nonperson_examples, engine = "regex")) == 0L,
  Moose_name_flag("Pt John Smith", engine = "regex") == 1L
)

patient_abbreviation_examples <- c(
  "Pt",
  "Pt Syncopal",
  "Pt Smith",
  "Pt John Smith",
  "Pt. Sarah Johnson"
)

stopifnot(
  identical(
    Moose_mask_person_names(patient_abbreviation_examples, engine = "regex"),
    c("Pt", "Pt Syncopal", "Pt [NAME]", "Pt [NAME]", "Pt. [NAME]")
  ),
  identical(
    Moose_name_flag(patient_abbreviation_examples, engine = "regex"),
    c(0L, 0L, 1L, 1L, 1L)
  ),
  identical(
    MooseR:::patient_abbreviation_adjusted_bounds("Pt Smith", 1L, 8L),
    list(start = 4L, end = 8L)
  ),
  is.null(MooseR:::patient_abbreviation_adjusted_bounds("Pt", 1L, 2L))
)

comma_name_examples <- c(
  "Smith, John",
  "SMITH, JOHN",
  "Smith, John Paul",
  "Smith, J.",
  "Smith,John",
  "O'Neil, Anne-Marie",
  "St-Pierre, Jean"
)

comma_nonperson_examples <- c(
  "Edmonton, Alberta",
  "Red Deer, Alberta",
  "Vancouver, British Columbia",
  "Vancouver, BC",
  "Avenue, Calgary",
  "AVENUE,CALGARY",
  "Street, Edmonton",
  "Route, Red Deer",
  "Avenue, Fort McMurry",
  "Pain, Chest"
)

comma_detected <- Moose_detect_person_names(comma_name_examples, engine = "regex")

stopifnot(
  identical(
    Moose_mask_person_names(comma_name_examples, engine = "regex"),
    rep("[NAME]", length(comma_name_examples))
  ),
  identical(
    Moose_name_flag(comma_name_examples, engine = "regex"),
    rep.int(1L, length(comma_name_examples))
  ),
  identical(comma_detected$detected_name, comma_name_examples),
  identical(
    Moose_apply_name_masking_rules(comma_name_examples),
    rep("[NAME]", length(comma_name_examples))
  ),
  identical(
    Moose_mask_person_names(comma_nonperson_examples, engine = "regex"),
    comma_nonperson_examples
  ),
  all(MooseR:::is_alberta_municipality_comma_candidate(c(
    "Avenue, Calgary",
    "AVENUE,CALGARY",
    "Street, Edmonton",
    "Route, Red Deer",
    "Avenue, Fort McMurry"
  ))),
  !MooseR:::is_alberta_municipality_comma_candidate("Smith, John"),
  identical(
    Moose_name_flag(comma_nonperson_examples, engine = "regex"),
    integer(length(comma_nonperson_examples))
  ),
  nrow(Moose_detect_person_names(comma_nonperson_examples, engine = "regex")) == 0L
)

known_name_records <- data.frame(
  comments = c(
    "ALICE SMITH called alice.",
    "Spoke to Tremblay, Jean-Paul.",
    "Joann spoke to o'neil.",
    "Heart Transplant was listed.",
    "ann arrived",
    "Status unknown",
    NA
  ),
  first_name = factor(c(
    "Alice", "Jean-Paul", "Ann", "Heart", "Bob", "Unknown", "Sarah"
  )),
  last_name = c(
    "Smith", "Tremblay", "O'Neil", "Transplant", "Jones", NA, "Brown"
  ),
  stringsAsFactors = FALSE
)

known_name_expected <- c(
  "[NAME] called [NAME].",
  "Spoke to [NAME].",
  "Joann spoke to [NAME].",
  "[NAME] was listed.",
  "ann arrived",
  "Status unknown",
  NA
)

known_name_masked <- Moose_mask_person_names(
  known_name_records$comments,
  engine = "regex",
  data = known_name_records,
  name_columns = c("first_name", "last_name")
)
known_name_flagged <- Moose_name_flag(
  known_name_records$comments,
  engine = "regex",
  data = known_name_records,
  name_columns = c("first_name", "last_name")
)

stopifnot(
  identical(known_name_masked, known_name_expected),
  identical(known_name_flagged, c(1L, 1L, 1L, 1L, 0L, 0L, 0L)),
  identical(
    Moose_mask_person_names(
      known_name_records$comments,
      engine = "regex",
      keep_original = TRUE,
      data = known_name_records,
      name_columns = c("first_name", "last_name")
    )$masked_text,
    known_name_expected
  ),
  inherits(
    tryCatch(
      Moose_name_flag(
        known_name_records$comments,
        engine = "regex",
        data = known_name_records,
        name_columns = "missing_name"
      ),
      error = identity
    ),
    "error"
  ),
  inherits(
    tryCatch(
      Moose_mask_person_names(
        known_name_records$comments,
        engine = "regex",
        name_columns = c("first_name", "last_name")
      ),
      error = identity
    ),
    "error"
  )
)

medical_abbreviation_examples <- c(
  "Hx Collected",
  "PMHx Reviewed",
  "HPI Updated",
  "ROS Completed",
  "Dx Pending",
  "Rx Updated",
  "Tx Started",
  "Sx Documented",
  "GCS Recorded",
  "SpO2 Recorded"
)

medical_abbreviations <- MooseR:::name_medical_abbreviations()

stopifnot(
  !anyDuplicated(toupper(medical_abbreviations)),
  all(vapply(
    medical_abbreviations,
    function(abbreviation) {
      MooseR:::is_nonperson_name_candidate(
        abbreviation,
        1L,
        nchar(abbreviation)
      )
    },
    logical(1)
  )),
  identical(
    Moose_mask_person_names(medical_abbreviation_examples, engine = "regex"),
    medical_abbreviation_examples
  ),
  identical(
    Moose_name_flag(medical_abbreviation_examples, engine = "regex"),
    integer(length(medical_abbreviation_examples))
  ),
  identical(
    Moose_mask_person_names(c("Dr Smith", "RN Johnson", "Paramedic Brown"), engine = "regex"),
    rep("[NAME]", 3L)
  )
)

medical_examples <- c(
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
  "Non Small Cell Lung",
  "Non-Small Cell Lung",
  "Situational Crisis",
  "Chief Complain",
  "Chief Complaint",
  "Chief Complaints",
  "Heart Transplant",
  "Liver Transplant",
  "Renal Transplant",
  "Bone Marrow Transplant",
  "Substance Abuse",
  "Substance Misuse",
  "Substance Dependence",
  "Substance Use Disorder",
  "Drug Addiction",
  "On Arrival",
  "Upon Arrival",
  "En Route",
  "Public Assist",
  "Covid Screen",
  "Year Old",
  "Years Old",
  "65 Year Old",
  "65 Years Old",
  "Patient is 65 Years Old",
  "Community Paramedic",
  "Orange Event",
  "Red Event",
  "Yellow Event",
  "On Scene",
  "Patient Discharged",
  "Patient Demographics",
  "Initial Presentation",
  "Interfacility Transfer",
  "Interfacility Transport",
  "Interfacility Call",
  "Interhospital Transfer",
  "Transport Truck",
  "Transport Cause",
  "Medical Transport Truck",
  "Patient Transport Truck",
  "Service Truck",
  "Ground Level",
  "Patient Found Ground Level",
  "Present Illness",
  "History Present Illness",
  "History of Present Illness",
  "Past Medical History",
  "Code Status",
  "Full Code",
  "No Code",
  "Resuscitation Status",
  "Nursing Home",
  "Skilled Nursing Facility",
  "Nursing Care Centre",
  "Nurse Practitioner",
  "Altered Mental Status",
  "Motor Vehicle Collision",
  "Ground Level Fall",
  "Right Arm Weakness",
  "Nausea Vomiting",
  "Fever Chills",
  "Septic Shock",
  "Allergic Reaction",
  "Blood Pressure",
  "Heart Rate",
  "Oxygen Saturation",
  "Suicidal Ideation",
  "Urinary Retention",
  "Urinary Issues",
  "Behavioural Problems",
  "Behavioral Problems",
  "Cervical Spine",
  "Facial Laceration",
  "Seizure Activity",
  "Nose Bleed",
  "Dog Bite",
  "Gastrointestinal Bleeding",
  "Opioid Overdose",
  "Alcohol Withdrawal",
  "Naloxone Infusion"
)

clinical_phrase_variants <- unique(c(
  MooseR:::name_clinical_phrases(),
  toupper(MooseR:::name_clinical_phrases()),
  tolower(MooseR:::name_clinical_phrases())
))

stopifnot(
  !anyDuplicated(MooseR:::name_clinical_terms()),
  !anyDuplicated(tolower(MooseR:::name_clinical_phrases())),
  all(MooseR:::is_clinical_phrase_candidate_values(
    MooseR:::name_clinical_phrases()
  )),
  all(MooseR:::is_clinical_phrase_candidate_values(toupper(
    MooseR:::name_clinical_phrases()
  ))),
  all(MooseR:::is_clinical_phrase_candidate_values(tolower(
    MooseR:::name_clinical_phrases()
  ))),
  MooseR:::is_clinical_phrase_span_values(
    "Non Small Cell Lung",
    1L,
    9L
  ),
  MooseR:::is_clinical_phrase_span_values(
    "Non Small Cell Lung",
    11L,
    19L
  ),
  identical(
    Moose_mask_person_names(clinical_phrase_variants, engine = "regex"),
    clinical_phrase_variants
  ),
  identical(
    Moose_name_flag(clinical_phrase_variants, engine = "regex"),
    integer(length(clinical_phrase_variants))
  ),
  nrow(Moose_detect_person_names(
    clinical_phrase_variants,
    engine = "regex"
  )) == 0L,
  MooseR:::is_nonperson_name_candidate("Urinary Issues", 9L, 14L),
  MooseR:::is_nonperson_name_candidate("Nose Bleed", 1L, 4L),
  MooseR:::is_nonperson_name_candidate("Nose Bleed", 6L, 10L),
  MooseR:::is_nonperson_name_candidate("Patient Demographics", 1L, 7L),
  MooseR:::is_nonperson_name_candidate("Patient Demographics", 9L, 20L),
  MooseR:::is_nonperson_name_candidate("Behavioural Problems", 1L, 11L),
  MooseR:::is_nonperson_name_candidate("Behavioural Problems", 13L, 20L),
  MooseR:::is_nonperson_name_candidate("Dog Bite", 1L, 3L),
  MooseR:::is_nonperson_name_candidate("Dog Bite", 5L, 8L),
  MooseR:::is_nonperson_name_candidate("En Route", 1L, 2L),
  MooseR:::is_nonperson_name_candidate("En Route", 4L, 8L),
  MooseR:::is_nonperson_name_candidate("Public Assist", 1L, 6L),
  MooseR:::is_nonperson_name_candidate("Public Assist", 8L, 13L),
  MooseR:::is_nonperson_name_candidate("Covid Screen", 1L, 5L),
  MooseR:::is_nonperson_name_candidate("Covid Screen", 7L, 12L),
  MooseR:::is_nonperson_name_candidate("Transport Cause", 1L, 9L),
  MooseR:::is_nonperson_name_candidate("Transport Cause", 11L, 15L),
  MooseR:::is_nonperson_name_candidate("Year Old", 1L, 4L),
  MooseR:::is_nonperson_name_candidate("Year Old", 6L, 8L),
  MooseR:::is_nonperson_name_candidate("Community Paramedic", 1L, 19L),
  !MooseR:::is_nonperson_name_candidate("Paramedic Smith", 1L, 15L),
  MooseR:::is_nonperson_name_candidate("Orange Event", 8L, 12L),
  !MooseR:::is_nonperson_name_candidate("Red Smith", 1L, 9L),
  identical(
    Moose_mask_person_names(medical_examples, engine = "regex"),
    medical_examples
  ),
  identical(
    Moose_name_flag(medical_examples, engine = "regex"),
    integer(length(medical_examples))
  ),
  nrow(Moose_detect_person_names(medical_examples, engine = "regex")) == 0L,
  identical(
    Moose_mask_person_names(
      c("John Smith", "Sarah Johnson"),
      engine = "regex"
    ),
    c("[NAME]", "[NAME]")
  )
)
