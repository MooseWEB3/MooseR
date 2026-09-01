library(MooseR)

local({
old_lc_time <- Sys.getlocale("LC_TIME")
on.exit(Sys.setlocale("LC_TIME", old_lc_time), add = TRUE)
Sys.setlocale("LC_TIME", "C")

two_digit_dates <- Moose_todate(c(
  "09/01/26",
  "Sep 1, 26",
  "01-Sep-26",
  "260901"
))

stopifnot(
  inherits(two_digit_dates, "Date"),
  identical(
    two_digit_dates,
    rep(as.Date("2026-09-01"), 4L)
  )
)

boosted_custom_cutoff <- Moose_boost_data(
  data.frame(visit_date = c("09/01/26", "09/02/26")),
  two_digit_year_cutoff = 20,
  verbose = FALSE
)
stopifnot(
  identical(
    format(boosted_custom_cutoff$visit_date, "%Y-%m-%d"),
    c("1926-09-01", "1926-09-02")
  )
)

numeric_short_date <- Moose_todate(260901)
stopifnot(identical(numeric_short_date, as.Date("2026-09-01")))

day_first_date <- Moose_todate("01/09/26", day_first = TRUE)
stopifnot(identical(day_first_date, as.Date("2026-09-01")))

two_digit_datetimes <- Moose_todatetime(c(
  "09/01/26 13:45:30",
  "Sep 1, 26 13:45:30"
))

stopifnot(
  inherits(two_digit_datetimes, "POSIXct"),
  identical(
    format(two_digit_datetimes, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    rep("2026-09-01 13:45:30", 2L)
  )
)

pivot_dates <- Moose_todate(c("01/01/68", "01/01/69"))
stopifnot(
  identical(
    format(pivot_dates, "%Y-%m-%d"),
    c("2068-01-01", "1969-01-01")
  )
)

custom_cutoff <- Moose_todate(
  "09/01/26",
  two_digit_year_cutoff = 20
)
stopifnot(identical(custom_cutoff, as.Date("1926-09-01")))

cutoff_error <- tryCatch(
  Moose_todate("09/01/26", two_digit_year_cutoff = 100),
  error = identity
)
stopifnot(
  inherits(cutoff_error, "error"),
  identical(
    conditionMessage(cutoff_error),
    "`two_digit_year_cutoff` must be one whole number from 0 to 99."
  )
)

four_digit_year <- Moose_todate(c("09/01/2026", "Sep 1, 2026"))
stopifnot(
  identical(
    four_digit_year,
    rep(as.Date("2026-09-01"), 2L)
  )
)

boosted <- Moose_boost_data(
  data.frame(
    visit_date = c("09/01/26", "09/02/26"),
    created_at = c("09/01/26 13:45", "09/02/26 08:15"),
    stringsAsFactors = FALSE
  ),
  verbose = FALSE
)

stopifnot(
  inherits(boosted$visit_date, "Date"),
  inherits(boosted$created_at, "POSIXct"),
  identical(
    format(boosted$visit_date, "%Y-%m-%d"),
    c("2026-09-01", "2026-09-02")
  ),
  identical(
    format(boosted$created_at, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    c("2026-09-01 13:45:00", "2026-09-02 08:15:00")
  )
)
})
