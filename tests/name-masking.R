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

medical_examples <- c(
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
  "On Scene",
  "Patient Discharged",
  "Patient Demographics",
  "Initial Presentation",
  "Interfacility Transfer",
  "Interfacility Transport",
  "Interfacility Call",
  "Interhospital Transfer",
  "Transport Truck",
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

stopifnot(
  !anyDuplicated(MooseR:::name_clinical_terms()),
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
