library(MooseR)

# Exercise both lazy loading and explicit data() loading in an installed package.
hospitals <- MooseR::alberta_hospitals
loaded <- new.env(parent = emptyenv())
data("alberta_hospitals", package = "MooseR", envir = loaded)
stopifnot(
  identical(hospitals, loaded$alberta_hospitals),
  is.data.frame(hospitals),
  identical(dim(hospitals), c(112L, 14L)),
  !anyNA(hospitals),
  !anyDuplicated(hospitals$facility_id),
  !anyDuplicated(hospitals$hospital_name),
  is.character(hospitals$facility_id),
  is.double(hospitals$latitude),
  is.double(hospitals$longitude),
  is.integer(hospitals$emergency_24h),
  all(hospitals$emergency_24h %in% c(0L, 1L)),
  sum(hospitals$emergency_24h == 1L) == 94L,
  sum(hospitals$emergency_24h == 0L) == 18L,
  inherits(hospitals$retrieved_on, "Date"),
  all(hospitals$retrieved_on == as.Date("2026-10-12")),
  all(hospitals$province == "Alberta"),
  all(grepl("^T[0-9][A-Z] [0-9][A-Z][0-9]$", hospitals$postal_code)),
  all(is.finite(hospitals$latitude)),
  all(is.finite(hospitals$longitude)),
  all(hospitals$latitude >= 49 & hospitals$latitude <= 60),
  all(hospitals$longitude >= -120 & hospitals$longitude <= -110),
  sum(hospitals$inclusion_basis == "ahs_hospitals_service") == 104L,
  sum(hospitals$inclusion_basis == "specialist_supplement") == 8L,
  !any(grepl("Lloydminster|Dawson Creek", hospitals$hospital_name))
)

# Guard the scope against accidentally omitting specialty facilities or using
# the obsolete hospital lists that predate the new Calgary/Grande Prairie sites.
stopifnot(all(c(
  "Arthur J.E. Child Comprehensive Cancer Centre",
  "Grande Prairie Regional Hospital", "Alberta Hospital Edmonton",
  "Centennial Centre for Mental Health and Brain Injury",
  "Claresholm Centre for Mental Health & Addictions",
  "Southern Alberta Forensic Psychiatry Centre", "Villa Caritas",
  "Lois Hole Hospital for Women", "Mazankowski Alberta Heart Institute",
  "Cross Cancer Institute", "Stollery Children's Hospital",
  "University of Alberta Hospital"
) %in% hospitals$hospital_name))

# Geographic checks catch reversed coordinates and incorrect city joins.
edmonton <- subset(hospitals, city == "Edmonton")
calgary <- subset(hospitals, city == "Calgary")
stopifnot(
  nrow(edmonton) == 12L, nrow(calgary) == 7L,
  all(edmonton$latitude > 53.4 & edmonton$latitude < 53.7),
  all(edmonton$longitude > -113.7 & edmonton$longitude < -113.3),
  all(calgary$latitude > 50.8 & calgary$latitude < 51.2),
  all(calgary$longitude > -114.3 & calgary$longitude < -113.8)
)

# The installed, human-readable snapshot must reproduce every data value.
csv_path <- system.file("extdata", "alberta-hospitals.csv", package = "MooseR")
stopifnot(nzchar(csv_path))
snapshot <- read.csv(csv_path, colClasses = "character", fileEncoding = "UTF-8")
snapshot$latitude <- as.numeric(snapshot$latitude)
snapshot$longitude <- as.numeric(snapshot$longitude)
snapshot$retrieved_on <- as.Date(snapshot$retrieved_on)
snapshot$emergency_24h <- as.integer(snapshot$emergency_24h)
stopifnot(identical(hospitals, snapshot))

# Hospital-wide 24-hour opening and co-location must not imply a 24-hour ED.
flags <- setNames(hospitals$emergency_24h, hospitals$hospital_name)
stopifnot(
  all(flags[c("Peter Lougheed Centre", "Royal Alexandra Hospital",
              "Alberta Children's Hospital", "Stollery Children's Hospital")] == 1L),
  all(flags[c("Alberta Hospital Edmonton", "Glenrose Rehabilitation Hospital",
              "Lois Hole Hospital for Women", "Mazankowski Alberta Heart Institute",
              "Claresholm Centre for Mental Health & Addictions")] == 0L),
  all(flags[c("Boyle Healthcare Centre", "Consort Hospital and Care Centre",
              "Grimshaw / Berwyn and District Community Health Centre",
              "Hardisty Health Centre", "Lamont Health Care Centre",
              "Swan Hills Healthcare Centre", "Two Hills Health Centre")] == 0L),
  # A short-term service advisory is not a change to the regular schedule.
  flags[["Coronation Hospital and Care Centre"]] == 1L
)

evidence <- read.csv(system.file(
  "extdata", "alberta-hospitals-emergency.csv", package = "MooseR"
), colClasses = "character", fileEncoding = "UTF-8")
stopifnot(
  nrow(evidence) == nrow(hospitals),
  !anyNA(evidence), all(nzchar(as.matrix(evidence))),
  !anyDuplicated(evidence$facility_id),
  setequal(evidence$facility_id, hospitals$facility_id),
  sum(evidence$assessment == "limited_hours_ed") == 7L,
  sum(evidence$assessment == "no_ed") == 11L,
  all((evidence$emergency_24h == "1") == (evidence$assessment == "24h_ed")),
  all(grepl("^https://www\\.albertahealthservices\\.ca/", evidence$source_url)),
  all(as.Date(evidence$checked_on) == as.Date("2026-10-12"))
)
evidence <- evidence[match(hospitals$facility_id, evidence$facility_id), ]
stopifnot(
  identical(evidence$hospital_name, hospitals$hospital_name),
  identical(as.integer(evidence$emergency_24h), hospitals$emergency_24h)
)

# Existing datasets retain their original public shapes.
stopifnot(
  is.list(MooseR::alberta_health_facilities),
  sum(lengths(MooseR::alberta_health_facilities)) == 1150L,
  is.list(MooseR::alberta_municipalities),
  sum(lengths(MooseR::alberta_municipalities)) == 334L
)
