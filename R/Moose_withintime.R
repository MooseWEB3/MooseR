#' Flag pairs of date-times within a time interval
#'
#' Returns 1 when the absolute elapsed time between two values is at most
#' `threshold`, and 0 otherwise. Missing or unparseable times return 0.
#' Character input is sampled to infer date order before all rows are parsed.
#'
#' @param time1,time2 Date-time vectors to compare row by row. Supports
#'   `POSIXct`, `POSIXlt`, `Date`, character, factor, integer and numeric
#'   vectors. A logical vector containing only `NA` is also accepted. Inputs
#'   must have equal lengths, or one must have length one and is repeated.
#' @param threshold One finite, non-negative number giving the maximum allowed
#'   absolute time difference, in `unit`. The boundary is included.
#' @param unit One of `"hour"` (default), `"day"`, `"minute"`, or `"second"`.
#'   A day is exactly 24 elapsed hours, including across daylight-saving changes.
#' @param sample_size Positive whole number. The first this many non-missing
#'   character values in each input are inspected separately to infer
#'   day/month order. Default: 50. This limits inference, not the rows compared.
#' @param tz Time zone for text without an explicit UTC offset and for `Date`
#'   values interpreted at midnight. Default: `"UTC"`. Use, for example,
#'   `"America/Edmonton"` for Alberta local time. Existing `POSIXt` instants
#'   and explicit offsets are preserved.
#' @param day_first Logical fallback for ambiguous text dates. Unambiguous
#'   sampled dates such as `13/02/2026` or `02/13/2026` determine the preferred
#'   order. If evidence is absent or tied, `FALSE` (default) prefers
#'   month/day/year and `TRUE` prefers day/month/year.
#' @param numeric_origin One of `"auto"`, `"excel"`, `"unix"`, or `"r"`.
#'   Passed to the numeric conversion used by [Moose_todatetime()]. Specify
#'   the origin when known; automatic inference cannot distinguish all numeric
#'   date representations.
#' @param two_digit_year_cutoff Whole number from 0 to 99, as in
#'   [Moose_todatetime()]. Default: 68. Applies to input being parsed, not
#'   existing `Date` or `POSIXt` values.
#' @return An integer vector containing only `1L` and `0L`, in input order.
#'   Missing values in either input, including two missing values, return
#'   `0L`. Names come from an input of the result's length, preferring `time1`
#'   when both are named. Two zero-length inputs, or a zero-length input and
#'   a scalar, return `integer(0)`.
#' @details
#' The comparison is `abs(time1 - time2) <= threshold` after conversion to
#' the chosen unit. Fractional thresholds are supported; the difference is
#' not rounded. A threshold of zero requires identical instants.
#'
#' Text uses the formats supported by [Moose_todatetime()], including ISO
#' dates, slash-separated dates, month names, compact dates, times with
#' seconds or fractional seconds, and ISO UTC offsets. Different supported
#' formats can occur after the sample; all rows are parsed. When day/month
#' order remains ambiguous, set `day_first` or convert the times explicitly
#' before calling this function. Month names follow the R `LC_TIME` locale.
#'
#' `NA`, `NaN`, blank text and the text markers `NA`, `N/A`, `NULL`, and
#' `NONE` (ignoring case) count as missing. A non-missing value that cannot
#' be parsed, including an infinite numeric value, returns 0 with a warning
#' giving the affected input and count. Unsupported input types are errors.
#' No input columns are modified.
#' @seealso [Moose_todatetime()], [Moose_todate()]
#' @examples
#' Moose_withintime("2026-10-14 09:00", "2026-10-14 12:00", 3) # 1
#' Moose_withintime("2026-10-14 09:00", "2026-10-14 12:01", 3) # 0
#' Moose_withintime("2026-10-14 09:00", "2026-10-14 09:30", 30,
#'                  unit = "minute") # 1
#' Moose_withintime(NA, "2026-10-14 09:00", 3) # 0
#'
#' visits <- data.frame(
#'   arrival = c("2026-10-14 09:00", "2026-10-14 09:00", NA),
#'   assessment = c("2026-10-14 11:00", "2026-10-14 13:00", "2026-10-14 10:00")
#' )
#' visits$within_3h <- Moose_withintime(
#'   visits$arrival, visits$assessment, 3, tz = "America/Edmonton"
#' )
#' @export
Moose_withintime <- function(time1, time2, threshold,
                             unit = c("hour", "day", "minute", "second"),
                             sample_size = 50L, tz = "UTC", day_first = FALSE,
                             numeric_origin = c("auto", "excel", "unix", "r"),
                             two_digit_year_cutoff = 68L) {
  if (!is.numeric(threshold) || is.complex(threshold) || is.object(threshold) ||
      !is.null(dim(threshold)) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold < 0) {
    stop("`threshold` must be one finite, non-negative number.", call. = FALSE)
  }
  unit <- match.arg(unit)
  if (!is.numeric(sample_size) || is.complex(sample_size) ||
      is.object(sample_size) || !is.null(dim(sample_size)) ||
      length(sample_size) != 1L || !is.finite(sample_size) ||
      sample_size < 1 || sample_size != floor(sample_size)) {
    stop("`sample_size` must be one positive whole number.", call. = FALSE)
  }
  tz <- moose_validate_tz(tz)
  if (!tz %in% c("UTC", "GMT", OlsonNames())) {
    stop("`tz` must be UTC, GMT, or a time zone from OlsonNames().", call. = FALSE)
  }
  day_first <- moose_validate_single_logical(day_first, "day_first")
  numeric_origin <- match.arg(numeric_origin)
  two_digit_year_cutoff <- moose_validate_two_digit_year_cutoff(two_digit_year_cutoff)

  n1 <- length(time1)
  n2 <- length(time2)
  if (n1 != n2 && n1 != 1L && n2 != 1L) {
    stop("`time1` and `time2` must have equal lengths, or one must have length one.",
         call. = FALSE)
  }
  size <- if (n1 == 0L || n2 == 0L) 0L else max(n1, n2)
  result_names <- if (n1 == size && !is.null(names(time1))) {
    names(time1)
  } else if (n2 == size) {
    names(time2)
  } else {
    NULL
  }

  first <- moose_withintime_datetime(time1, "time1", sample_size, tz,
                                    day_first, numeric_origin, two_digit_year_cutoff)
  second <- moose_withintime_datetime(time2, "time2", sample_size, tz,
                                     day_first, numeric_origin, two_digit_year_cutoff)
  if (size == 0L) return(integer(0))
  if (n1 == 1L) first <- rep(first, size)
  if (n2 == 1L) second <- rep(second, size)

  seconds_per_unit <- switch(unit, day = 86400, hour = 3600, minute = 60, second = 1)
  delta <- abs(as.numeric(difftime(first, second, units = "secs"))) / seconds_per_unit
  result <- as.integer(is.finite(delta) & delta <= threshold)
  names(result) <- result_names
  result
}

moose_withintime_datetime <- function(x, argument, sample_size, tz,
                                      day_first, numeric_origin, cutoff) {
  supported <- inherits(x, "POSIXt") || inherits(x, "Date") || is.factor(x) ||
    (!is.object(x) && (is.character(x) || (is.numeric(x) && !is.complex(x)) ||
                      (is.logical(x) && all(is.na(x)))))
  if (!supported || !is.null(dim(x))) {
    stop("`", argument,
         "` must be a POSIXt, Date, character, factor, or numeric vector.",
         call. = FALSE)
  }
  missing <- is.na(x)
  if (inherits(x, "POSIXt")) {
    # Preserve known instants, including their year and daylight-saving offset.
    result <- as.POSIXct(x)
  } else if (inherits(x, "Date")) {
    # as.POSIXct.Date uses UTC midnight even with tz set; parse calendar dates
    # explicitly so midnight is in the requested zone instead.
    result <- as.POSIXct(format(x, "%Y-%m-%d"), format = "%Y-%m-%d", tz = tz)
  } else if (is.character(x) || is.factor(x)) {
    values <- trimws(as.character(x))
    missing <- is.na(values) | !nzchar(values) |
      tolower(values) %in% c("na", "n/a", "null", "none")
    result <- moose_character_to_datetime(
      values, day_first = day_first, numeric_origin = numeric_origin, tz = tz,
      two_digit_year_cutoff = cutoff, sample_size = sample_size
    )
  } else if (is.logical(x)) {
    result <- moose_posix_na(length(x), tz)
  } else {
    result <- Moose_todatetime(x, numeric_origin = numeric_origin, tz = tz,
                               two_digit_year_cutoff = cutoff)
  }
  invalid <- !missing & !is.finite(as.numeric(result))
  if (any(invalid)) {
    warning("`", argument, "`: ", sum(invalid),
            " non-missing value(s) could not be converted to a finite date-time;",
            " returning 0 for affected pairs.", call. = FALSE)
  }
  result
}
