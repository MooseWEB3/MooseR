# Rebuild data/alberta_municipalities.rda from the canonical municipality list.
# Run this script from the package root after updating R/alberta_municipalities.R.

source(file.path("R", "alberta_municipalities.R"))

municipalities <- alberta_official_municipalities()
category_sizes <- c(
  cities = 19L,
  specialized_municipalities = 6L,
  municipal_districts = 63L,
  towns = 105L,
  villages = 80L,
  summer_villages = 51L,
  improvement_districts = 7L,
  special_areas = 3L
)

stopifnot(
  length(municipalities) == sum(category_sizes),
  !anyDuplicated(municipalities)
)

alberta_municipalities <- split(
  municipalities,
  factor(
    rep(names(category_sizes), category_sizes),
    levels = names(category_sizes)
  )
)

dir.create("data", showWarnings = FALSE)
save(
  alberta_municipalities,
  file = file.path("data", "alberta_municipalities.rda"),
  compress = "xz",
  version = 2
)
