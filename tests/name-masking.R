library(MooseR)

facilities <- MooseR:::alberta_health_facilities()

stopifnot(
  length(facilities) == 1150L,
  !anyDuplicated(facilities)
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
  "Heavily Intoxicated"
)

stopifnot(
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
