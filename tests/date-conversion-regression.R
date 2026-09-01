library(MooseR)

local({
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_lc_time), add = TRUE)
  Sys.setlocale("LC_TIME", "C")

  two_digit_dates <- Moose_todate(c(
    "09/01/26",
    "Sep 1, 26",
    "01-Sep-26",
    "1 September 26",
    "260901"
  ))

  stopifnot(
    inherits(two_digit_dates, "Date"),
    identical(two_digit_dates, rep(as.Date("2026-09-01"), 5L)),
    identical(Moose_todate(260901), as.Date("2026-09-01"))
  )

  ambiguous_default <- Moose_todate("01/02/26")
  ambiguous_day_first <- Moose_todate("01/02/26", day_first = TRUE)
  stopifnot(
    identical(ambiguous_default, as.Date("2026-01-02")),
    identical(ambiguous_day_first, as.Date("2026-02-01"))
  )

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

  default_boundaries <- Moose_todate(c(
    "01/01/00",
    "01/01/68",
    "01/01/69",
    "01/01/99"
  ))
  stopifnot(identical(
    format(default_boundaries, "%Y-%m-%d"),
    c("2000-01-01", "2068-01-01", "1969-01-01", "1999-01-01")
  ))

  custom_dates <- Moose_todate(
    c("01/01/30", "01/01/31"),
    two_digit_year_cutoff = 30
  )
  custom_datetimes <- Moose_todatetime(
    c("01/01/30 01:02", "01/01/31 01:02"),
    two_digit_year_cutoff = 30
  )
  stopifnot(
    identical(
      format(custom_dates, "%Y-%m-%d"),
      c("2030-01-01", "1931-01-01")
    ),
    identical(
      format(custom_datetimes, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      c("2030-01-01 01:02:00", "1931-01-01 01:02:00")
    ),
    identical(
      Moose_todate("01/01/01", two_digit_year_cutoff = 0),
      as.Date("1901-01-01")
    ),
    identical(
      Moose_todate("01/01/99", two_digit_year_cutoff = 99),
      as.Date("2099-01-01")
    )
  )

  four_digit_dates <- Moose_todate(c(
    "09/01/2026",
    "Sep 1, 2026",
    "01-Sep-2026",
    "2026-09-01"
  ))
  four_digit_datetime <- Moose_todatetime("01-Sep-2026 13:45:30")
  four_digit_extreme_cutoff <- Moose_todate(
    "09/01/2026",
    two_digit_year_cutoff = 0
  )
  four_digit_datetime_extreme_cutoff <- Moose_todatetime(
    "09/01/2026 13:45:30",
    two_digit_year_cutoff = 99
  )
  stopifnot(
    identical(four_digit_dates, rep(as.Date("2026-09-01"), 4L)),
    identical(four_digit_extreme_cutoff, as.Date("2026-09-01")),
    identical(
      format(four_digit_datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      "2026-09-01 13:45:30"
    ),
    identical(
      format(
        four_digit_datetime_extreme_cutoff,
        "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      "2026-09-01 13:45:30"
    )
  )

  compact_datetimes <- Moose_todatetime(c(202601021330, 20260102133045))
  stopifnot(identical(
    format(compact_datetimes, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    c("2026-01-02 13:30:00", "2026-01-02 13:30:45")
  ))

  offset_datetime <- Moose_todatetime(
    "2026-01-01T12:30:00+01:00",
    tz = "UTC"
  )
  utc_datetime <- Moose_todatetime(
    "2026-01-01T12:30:00Z",
    tz = "America/Edmonton"
  )
  stopifnot(
    identical(
      format(offset_datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      "2026-01-01 11:30:00"
    ),
    identical(
      format(utc_datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      "2026-01-01 12:30:00"
    )
  )

  cross_century_datetime <- Moose_todatetime(
    "03/14/21 02:30",
    tz = "America/Edmonton",
    two_digit_year_cutoff = 20
  )
  stopifnot(identical(
    format(
      cross_century_datetime,
      "%Y-%m-%d %H:%M:%S",
      tz = "America/Edmonton"
    ),
    "1921-03-14 02:30:00"
  ))

  malformed_datetimes <- Moose_todatetime(c(
    "2026-01-01 nonsense",
    "09/01/26 garbage",
    "2026-01-01 12:30junk",
    "2026-01-01 25:00",
    "09/01/26T12:30"
  ))
  stopifnot(all(is.na(malformed_datetimes)))

  factor_dates <- Moose_todate(factor(c("01-Jan-26", NA_character_)))
  invalid_date <- Moose_todate("02/30/26")
  stopifnot(
    identical(factor_dates[1L], as.Date("2026-01-01")),
    is.na(factor_dates[2L]),
    is.na(invalid_date)
  )

  expected_cutoff_error <- paste0(
    "`two_digit_year_cutoff` must be one whole number from 0 to 99."
  )
  invalid_cutoffs <- list(
    NA_real_, Inf, -1, 100, 1.5, TRUE, "68", c(10, 20)
  )

  for (bad_cutoff in invalid_cutoffs) {
    date_error <- tryCatch(
      Moose_todate("01/01/26", two_digit_year_cutoff = bad_cutoff),
      error = identity
    )
    datetime_error <- tryCatch(
      Moose_todatetime(
        "01/01/26 01:02",
        two_digit_year_cutoff = bad_cutoff
      ),
      error = identity
    )
    stopifnot(
      inherits(date_error, "error"),
      identical(conditionMessage(date_error), expected_cutoff_error),
      inherits(datetime_error, "error"),
      identical(conditionMessage(datetime_error), expected_cutoff_error)
    )
  }

  boosted <- Moose_boost_data(
    data.frame(
      visit_date = c("09/01/26", "09/02/26"),
      created_at = c("09/01/26 13:45", "09/02/26 08:15"),
      compact_date = c("260901", "260902"),
      code = c("260901", "260902"),
      stringsAsFactors = FALSE
    ),
    verbose = FALSE
  )

  stopifnot(
    inherits(boosted$visit_date, "Date"),
    inherits(boosted$created_at, "POSIXct"),
    inherits(boosted$compact_date, "Date"),
    is.character(boosted$code),
    identical(boosted$code, c("260901", "260902")),
    identical(
      format(boosted$visit_date, "%Y-%m-%d"),
      c("2026-09-01", "2026-09-02")
    ),
    identical(
      format(boosted$created_at, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      c("2026-09-01 13:45:00", "2026-09-02 08:15:00")
    ),
    identical(
      format(boosted$compact_date, "%Y-%m-%d"),
      c("2026-09-01", "2026-09-02")
    )
  )

  boosted_custom_cutoff <- Moose_boost_data(
    data.frame(
      visit_date = c("09/01/26", "09/02/26"),
      created_at = c("09/01/26 13:45", "09/02/26 08:15")
    ),
    verbose = FALSE,
    two_digit_year_cutoff = 20
  )
  boost_cutoff_error <- tryCatch(
    Moose_boost_data(
      data.frame(visit_date = "09/01/26"),
      verbose = FALSE,
      two_digit_year_cutoff = 100
    ),
    error = identity
  )
  positional_boost <- Moose_boost_data(
    data.frame(code = c("260901", "260902")),
    FALSE,
    "auto",
    "UTC",
    0.8,
    FALSE
  )
  stopifnot(
    identical(
      format(boosted_custom_cutoff$visit_date, "%Y-%m-%d"),
      c("1926-09-01", "1926-09-02")
    ),
    identical(
      format(
        boosted_custom_cutoff$created_at,
        "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      c("1926-09-01 13:45:00", "1926-09-02 08:15:00")
    ),
    inherits(boost_cutoff_error, "error"),
    identical(conditionMessage(boost_cutoff_error), expected_cutoff_error),
    identical(positional_boost$code, c("260901", "260902"))
  )
})
