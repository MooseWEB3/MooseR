library(MooseR)

stopifnot(
  identical(Moose_second2format(1000), "00:16:40"),
  identical(Moose_second2format(c(0, 1, 59, 60, 3599, 3600, 3661)),
            c("00:00:00", "00:00:01", "00:00:59", "00:01:00",
              "00:59:59", "01:00:00", "01:01:01")),
  # Durations are not clock times: they must not wrap at 24 hours or overflow
  # when seconds exceed R's signed 32-bit integer range.
  identical(Moose_second2format(c(86400, 90061, 360000, 2147483648)),
            c("24:00:00", "25:01:01", "100:00:00", "596523:14:08")),
  identical(Moose_second2format(c(0.49, 0.5, 2.5, 59.5, 3599.5, 86399.5)),
            c("00:00:00", "00:00:01", "00:00:03", "00:01:00", "01:00:00", "24:00:00")),
  identical(Moose_second2format(c(first = 1000L, missing = NA_integer_)),
            c(first = "00:16:40", missing = NA_character_)),
  identical(Moose_second2format(c(NA_real_, NaN, 1000)),
            c(NA_character_, NA_character_, "00:16:40")),
  identical(Moose_second2format(NA), NA_character_),
  identical(Moose_second2format(numeric()), character())
)

trips <- data.frame(travel_time_seconds = c(1000, 3661, NA))
trips$formatted <- Moose_second2format(trips$travel_time_seconds)
stopifnot(identical(trips$formatted, c("00:16:40", "01:01:01", NA_character_)))

for (invalid in list(-1, Inf, -Inf, "1000", TRUE, 1 + 1i,
                     list(1000), matrix(1000), as.Date("2026-10-12"))) {
  error <- tryCatch(Moose_second2format(invalid), error = identity)
  stopifnot(inherits(error, "error"), grepl("seconds", conditionMessage(error), fixed = TRUE))
}
