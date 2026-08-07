# Rebuild data/alberta_health_facilities.rda from the bundled AHS snapshot.
# Run this script from the package root after updating the source text file.

source_path <- file.path(
  "inst",
  "extdata",
  "alberta-health-facilities.txt"
)

facilities <- readLines(source_path, encoding = "UTF-8", warn = FALSE)
facilities <- unique(trimws(facilities[nzchar(trimws(facilities))]))

initial <- toupper(substr(facilities, 1L, 1L))
initial[grepl("^[0-9]$", initial)] <- "0-9"
initial[!grepl("^[A-Z]$|^0-9$", initial)] <- "Other"
group_order <- c("0-9", LETTERS, "Other")

alberta_health_facilities <- split(
  facilities,
  factor(initial, levels = group_order)
)
alberta_health_facilities <- alberta_health_facilities[
  lengths(alberta_health_facilities) > 0L
]

stopifnot(
  length(facilities) == 1150L,
  !anyDuplicated(facilities),
  sum(lengths(alberta_health_facilities)) == 1150L
)

dir.create("data", showWarnings = FALSE)
save(
  alberta_health_facilities,
  file = file.path("data", "alberta_health_facilities.rda"),
  compress = "xz",
  version = 2
)
