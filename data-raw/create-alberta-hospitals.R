# Rebuild the offline dataset from the reviewed UTF-8 AHS snapshot.
# Run from the package root. No network access or contributed R packages needed.
# Refresh first with: python3 data-raw/update-alberta-hospitals.py

alberta_hospitals <- read.csv(
  file.path("inst", "extdata", "alberta-hospitals.csv"),
  colClasses = "character", fileEncoding = "UTF-8",
  stringsAsFactors = FALSE, check.names = FALSE, na.strings = ""
)

expected_columns <- c(
  "facility_id", "hospital_name", "address", "street_address", "city",
  "province", "postal_code", "latitude", "longitude", "inclusion_basis",
  "inclusion_source_url", "source_url", "retrieved_on", "emergency_24h"
)
stopifnot(
  identical(names(alberta_hospitals), expected_columns),
  nrow(alberta_hospitals) >= 100L,
  !anyNA(alberta_hospitals),
  all(nzchar(as.matrix(alberta_hospitals))),
  !anyDuplicated(alberta_hospitals$facility_id),
  !anyDuplicated(alberta_hospitals$hospital_name),
  all(grepl("^[0-9]+$", alberta_hospitals$facility_id)),
  all(alberta_hospitals$province == "Alberta"),
  all(alberta_hospitals$emergency_24h %in% c("0", "1")),
  all(grepl("^T[0-9][A-Z] [0-9][A-Z][0-9]$", alberta_hospitals$postal_code)),
  all(alberta_hospitals$inclusion_basis %in%
        c("ahs_hospitals_service", "specialist_supplement")),
  all(grepl("^https://www\\.albertahealthservices\\.ca/findhealth/",
            alberta_hospitals$source_url))
)

alberta_hospitals$latitude <- as.numeric(alberta_hospitals$latitude)
alberta_hospitals$longitude <- as.numeric(alberta_hospitals$longitude)
alberta_hospitals$retrieved_on <- as.Date(alberta_hospitals$retrieved_on)
alberta_hospitals$emergency_24h <- as.integer(alberta_hospitals$emergency_24h)

# ED schedules have their own sources and review date. A missing review must
# fail the build, never silently become "no 24-hour ED".
emergency_evidence <- read.csv(
  file.path("inst", "extdata", "alberta-hospitals-emergency.csv"),
  colClasses = "character", fileEncoding = "UTF-8", stringsAsFactors = FALSE
)
stopifnot(
  !anyNA(emergency_evidence),
  all(nzchar(as.matrix(emergency_evidence))),
  !anyDuplicated(emergency_evidence$facility_id),
  setequal(emergency_evidence$facility_id, alberta_hospitals$facility_id),
  all(emergency_evidence$emergency_24h %in% c("0", "1")),
  all(emergency_evidence$assessment %in% c("24h_ed", "limited_hours_ed", "no_ed")),
  all((emergency_evidence$emergency_24h == "1") ==
        (emergency_evidence$assessment == "24h_ed")),
  !anyNA(as.Date(emergency_evidence$checked_on))
)
emergency_evidence <- emergency_evidence[
  match(alberta_hospitals$facility_id, emergency_evidence$facility_id),
]
stopifnot(
  identical(alberta_hospitals$hospital_name, emergency_evidence$hospital_name),
  identical(alberta_hospitals$emergency_24h,
            as.integer(emergency_evidence$emergency_24h))
)
stopifnot(
  !anyNA(alberta_hospitals),
  all(is.finite(alberta_hospitals$latitude)),
  all(is.finite(alberta_hospitals$longitude)),
  all(alberta_hospitals$latitude >= 49 & alberta_hospitals$latitude <= 60),
  all(alberta_hospitals$longitude >= -120 & alberta_hospitals$longitude <= -110),
  length(unique(alberta_hospitals$retrieved_on)) == 1L,
  identical(alberta_hospitals$address, paste0(
    alberta_hospitals$street_address, ", ", alberta_hospitals$city, ", ",
    alberta_hospitals$province, " ", alberta_hospitals$postal_code
  ))
)

dir.create("data", showWarnings = FALSE)
save(alberta_hospitals, file = file.path("data", "alberta_hospitals.rda"),
     compress = "xz", version = 2)
message("Saved ", nrow(alberta_hospitals), " Alberta hospital records.")
