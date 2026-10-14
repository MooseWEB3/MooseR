library(MooseR)

base_time <- as.POSIXct("2026-10-14 09:00:00", tz = "UTC")
other_times <- base_time + c(0, 10800, -10800, 10801, -10801, NA)
stopifnot(
  identical(Moose_withintime(base_time, other_times, 3), c(1L, 1L, 1L, 0L, 0L, 0L)),
  identical(Moose_withintime(other_times, base_time, 3), c(1L, 1L, 1L, 0L, 0L, 0L)),
  identical(Moose_withintime(base_time, base_time + c(0, 0.5), 0), c(1L, 0L)),
  identical(Moose_withintime(base_time, base_time + c(0.5, 0.75), 0.5,
                            unit = "second"), c(1L, 0L)),
  identical(Moose_withintime(base_time, base_time + c(1800, 1801), 0.5), c(1L, 0L)),
  identical(Moose_withintime(as.POSIXlt(base_time), as.POSIXlt(other_times), 3),
            c(1L, 1L, 1L, 0L, 0L, 0L))
)

for (unit in c("day", "hour", "minute", "second")) {
  scale <- switch(unit, day = 86400, hour = 3600, minute = 60, second = 1)
  stopifnot(identical(
    Moose_withintime(base_time, base_time + c(-scale, scale, scale + 0.5), 1, unit),
    c(1L, 1L, 0L)
  ))
}

# Missing rows always return zero, with no warnings for known missing markers.
withCallingHandlers({
  missing_text <- c(NA, "", "  ", "NA", "n/a", "Null", "NONE")
  stopifnot(
    identical(Moose_withintime(missing_text, base_time, 3), rep(0L, 7)),
    identical(Moose_withintime(base_time, missing_text, 3), rep(0L, 7)),
    identical(Moose_withintime(NA, NA, 3), 0L),
    identical(Moose_withintime(c(NA_real_, NaN), base_time, 3), c(0L, 0L)),
    identical(Moose_withintime(as.Date(NA), base_time, 3), 0L),
    identical(Moose_withintime(as.POSIXct(NA), base_time, 3), 0L),
    identical(Moose_withintime(as.POSIXlt(NA), base_time, 3), 0L)
  )
}, warning = function(w) stop(w))

# Formats, offsets, factors, fractional seconds and numeric origins.
local({
  old_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_locale), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  text <- c("2026-10-14 09:00", "2026/10/14 09:00:00", "10/14/2026 9:00 AM",
            "14-Oct-26 09:00", "20261014090000", "2026-10-14T11:00:00+02:00",
            "2026-10-14T09:00:00Z")
  stopifnot(
    identical(Moose_withintime(text, base_time, 0), rep(1L, length(text))),
    identical(Moose_withintime(factor(text), base_time, 0), rep(1L, length(text))),
    identical(Moose_withintime("2026-10-14 09:00:00.5", base_time, 0.5,
                              unit = "second"), 1L),
    identical(Moose_withintime(as.numeric(base_time), base_time, 0), 1L),
    identical(Moose_withintime(202610140900, base_time, 0), 1L),
    identical(Moose_withintime(45000, 45000.125, 3, numeric_origin = "excel"), 1L),
    identical(Moose_withintime(0, 10800, 3, numeric_origin = "unix"), 1L),
    identical(Moose_withintime(0, 0.125, 3, numeric_origin = "r"), 1L),
    identical(Moose_withintime("01/01/30 09:00", "1930-01-01 09:00", 0,
                              two_digit_year_cutoff = 20), 1L)
  )
})

# Sample each input independently and parse every row, including later formats.
sample_input <- c(rep("01/02/2026 10:00", 30), "13/02/2026 10:00",
                  rep("2026-02-01T10:00:00Z", 29))
sample_reference <- c(rep("2026-02-01 10:00", 30), "2026-02-13 10:00",
                      rep("2026-02-01 10:00", 29))
stopifnot(
  identical(Moose_withintime(sample_input, sample_reference, 0), rep(1L, 60)),
  identical(Moose_withintime(sample_input, sample_reference, 0, sample_size = 30),
            c(rep(0L, 30), rep(1L, 30))),
  identical(Moose_withintime(sample_input, sample_reference, 0, sample_size = 30,
                            day_first = TRUE), rep(1L, 60)),
  identical(Moose_withintime(c(rep(NA, 50), sample_input),
                            c(rep(NA, 50), sample_reference), 0),
            c(rep(0L, 50), rep(1L, 60))),
  identical(Moose_withintime(c("01/02/2026", "13/02/2026"),
                            c("02/01/2026", "02/13/2026"), 0), c(1L, 1L)),
  identical(Moose_withintime("01/02/2026", "2026-01-02", 0), 1L),
  identical(Moose_withintime("01/02/2026", "2026-02-01", 0, day_first = TRUE), 1L)
)

# Compare instants across zones; local Date midnight and elapsed DST days.
local_time <- as.POSIXct("2026-10-14 09:00", tz = "America/Edmonton")
stopifnot(
  identical(Moose_withintime(local_time, "2026-10-14T15:00:00Z", 0), 1L),
  identical(Moose_withintime(local_time, "2026-10-14 09:00", 0,
                            tz = "America/Edmonton"), 1L),
  identical(Moose_withintime(as.Date("2026-10-14"), "2026-10-14T06:00:00Z", 0,
                            tz = "America/Edmonton"), 1L),
  identical(Moose_withintime("2026-03-08 01:30", "2026-03-08 03:30", 1,
                            tz = "America/Edmonton"), 1L),
  identical(Moose_withintime(as.Date("2025-11-02"), as.Date("2025-11-03"), 1,
                            unit = "day", tz = "America/Edmonton"), 0L),
  identical(Moose_withintime("2026-11-01T01:30:00-06:00",
                            "2026-11-01T01:30:00-07:00", 59, unit = "minute"), 0L)
)

# Warn once per input for bad values, while retaining valid/missing row results.
warnings <- character()
result <- withCallingHandlers(
  Moose_withintime(c("bad", "2026-02-30", "2026-10-14 09:00", NA),
                   c("also bad", "2026-10-14", "2026-10-14 10:00", NA), 3),
  warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
stopifnot(
  identical(result, c(0L, 0L, 1L, 0L)),
  length(warnings) == 2L,
  grepl("`time1`: 2", warnings[1], fixed = TRUE),
  grepl("`time2`: 1", warnings[2], fixed = TRUE),
  identical(suppressWarnings(Moose_withintime(c(Inf, -Inf), base_time, 3)), c(0L, 0L))
)

# Pairwise dataset use, names, empty inputs, and no mutation of inputs.
visits <- data.frame(arrival = c("2026-10-14 09:00", "2026-10-14 09:00", NA),
                      assessment = c("2026-10-14 11:00", "2026-10-14 13:00", NA))
original <- visits
visits$within_3h <- Moose_withintime(visits$arrival, visits$assessment, 3)
named_times <- c(first = "2026-10-14 09:00", second = "2026-10-14 13:00")
stopifnot(
  identical(visits$within_3h, c(1L, 0L, 0L)),
  identical(visits[names(original)], original),
  identical(Moose_withintime(named_times, base_time, 3), c(first = 1L, second = 0L)),
  identical(Moose_withintime(base_time, named_times, 3), c(first = 1L, second = 0L)),
  identical(Moose_withintime(character(), character(), 3), integer()),
  identical(Moose_withintime(character(), base_time, 3), integer()),
  identical(Moose_withintime(base_time, character(), 3), integer())
)

must_error <- function(expression, pattern) {
  error <- tryCatch(force(expression), error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error), fixed = TRUE))
}
for (bad in list(-1, Inf, NA, NaN, "3", TRUE, c(1, 2), numeric(), 1 + 1i,
                 matrix(3), as.difftime(3, units = "hours"))) {
  must_error(Moose_withintime(base_time, base_time, bad), "threshold")
}
for (bad in list(0, -1, 1.5, NA, Inf, "50", TRUE, c(30, 50), matrix(50))) {
  must_error(Moose_withintime(base_time, base_time, 3, sample_size = bad), "sample_size")
}
for (bad in list(TRUE, list(base_time), matrix(base_time), data.frame(t = base_time),
                 1 + 1i, as.difftime(3, units = "hours"))) {
  must_error(Moose_withintime(bad, base_time, 3), "time1")
  must_error(Moose_withintime(base_time, bad, 3), "time2")
}
must_error(Moose_withintime(rep(base_time, 2), rep(base_time, 3), 3), "equal lengths")
must_error(Moose_withintime(character(), rep(base_time, 2), 3), "equal lengths")
must_error(Moose_withintime(base_time, base_time, 3, unit = "week"), "arg")
must_error(Moose_withintime(base_time, base_time, 3, tz = "Not/AZone"), "tz")
must_error(Moose_withintime(base_time, base_time, 3, day_first = NA), "day_first")
must_error(Moose_withintime(base_time, base_time, 3, two_digit_year_cutoff = 100),
           "two_digit_year_cutoff")
