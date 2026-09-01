library(MooseR)

stopifnot(
  identical(Moose_todate("13-Jul-26"), as.Date("2026-07-13")),
  identical(
    format(Moose_todatetime("13-Jul-26"), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2026-07-13 00:00:00"
  )
)

year_0026_date <- as.Date("0026-07-13", format = "%Y-%m-%d")
year_0026_datetime <- as.POSIXct(
  strptime("0026-07-13 08:30:00", "%Y-%m-%d %H:%M:%S", tz = "UTC")
)

stopifnot(
  identical(Moose_todate(year_0026_date), as.Date("2026-07-13")),
  identical(
    format(Moose_todatetime(year_0026_datetime), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2026-07-13 08:30:00"
  )
)

inferred_day_first <- Moose_todate(c(
  "01/02/26",
  "13/02/26",
  "03/02/26"
))
inferred_month_first <- Moose_todate(
  c("01/02/26", "02/13/26", "03/04/26"),
  day_first = TRUE
)

stopifnot(
  identical(
    format(inferred_day_first, "%Y-%m-%d"),
    c("2026-02-01", "2026-02-13", "2026-02-03")
  ),
  identical(
    format(inferred_month_first, "%Y-%m-%d"),
    c("2026-01-02", "2026-02-13", "2026-03-04")
  )
)
