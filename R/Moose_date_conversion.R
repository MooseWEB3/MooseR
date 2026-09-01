#' Convert input to R Date or date-time values
#'
#' `Moose_todate()` converts common date inputs to `Date`.
#' `Moose_todatetime()` converts common date or date-time inputs to `POSIXct`.
#'
#' @param x Input vector. Supports `Date`, `POSIXt`, character, factor, integer,
#'   and numeric values.
#' @param day_first Logical fallback for ambiguous numeric dates. MooseR first
#'   examines the input vector for unambiguous values such as `13/02/2024` or
#'   `02/13/2024` and applies the better-supported order to the whole vector.
#'   If the evidence is tied or absent, `TRUE` prefers day/month/year and
#'   `FALSE` prefers month/day/year.
#' @param numeric_origin Character. One of `"auto"`, `"excel"`, `"unix"`, or
#'   `"r"`. In `"auto"` mode, compact values such as `20240131` and `240131`
#'   are parsed as `YYYYMMDD` and `YYMMDD`, large values are parsed as Unix
#'   seconds, Excel-like serial values are parsed using Excel's Windows origin,
#'   and smaller values are parsed as R days since 1970-01-01.
#' @param tz Time zone used for date-time parsing and conversion.
#' @param two_digit_year_cutoff Whole number from 0 to 99. Two-digit years at
#'   or below this value are interpreted as 2000s; larger values are
#'   interpreted as 1900s. The default, `68`, follows the POSIX/R convention.
#' @details With the default cutoff, two-digit years from `00` to `68` are
#'   interpreted as 2000 to 2068, and values from `69` to `99` are interpreted
#'   as 1969 to 1999. Four-digit years are never parsed as two-digit years.
#'   Prefer four-digit years whenever the century must be unambiguous.
#'
#'   Numeric dates with the year last are interpreted consistently across the
#'   input vector. MooseR counts unambiguous day-first and month-first values,
#'   then applies the better-supported order to ambiguous values. Ties use
#'   `day_first`.
#'
#'   Inputs already stored as `Date` or `POSIXt` with a year from 0000 to 0099
#'   are treated as having originated from a two-digit year and are normalized
#'   using `two_digit_year_cutoff`.
#'   Month-name inputs use the current R `LC_TIME` locale.
#'
#' @return `Moose_todate()` returns a `Date` vector. `Moose_todatetime()`
#'   returns a `POSIXct` vector.
#'
#' @examples
#' Moose_todate(c("2024-01-05", "01/06/2024", "20240107"))
#' Moose_todate(c("01/02/2026", "13/02/2026", "03/02/2026"))
#' Moose_todate("13-Jul-26")
#' Moose_todate(c("09/01/26", "Sep 1, 26", "260901"))
#' Moose_todate("09/01/26", two_digit_year_cutoff = 20)
#' Moose_todate(c(20240105, 45296))
#'
#' Moose_todatetime(c("2024-01-05 13:30:00", "2024/01/06 8:05"))
#' Moose_todatetime("09/01/26 13:30:00")
#' Moose_todatetime(c(202401051330, 1704450600))
#'
#' @export
Moose_todate <- function(x,
                         day_first = FALSE,
                         numeric_origin = c("auto", "excel", "unix", "r"),
                         tz = "UTC",
                         two_digit_year_cutoff = 68L) {
  numeric_origin <- match.arg(numeric_origin)
  day_first <- moose_validate_single_logical(day_first, "day_first")
  tz <- moose_validate_tz(tz)
  two_digit_year_cutoff <- moose_validate_two_digit_year_cutoff(
    two_digit_year_cutoff
  )

  if (is.null(x)) {
    return(as.Date(character()))
  }

  if (inherits(x, "Date")) {
    return(moose_normalize_date_year(x, two_digit_year_cutoff))
  }

  if (inherits(x, "POSIXt")) {
    normalized <- moose_normalize_datetime_year(
      x,
      two_digit_year_cutoff,
      tz
    )
    return(as.Date(normalized, tz = tz))
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.numeric(x) || is.integer(x)) {
    result <- moose_numeric_to_date(
      x,
      numeric_origin = numeric_origin,
      tz = tz,
      two_digit_year_cutoff = two_digit_year_cutoff
    )
    return(moose_normalize_date_year(result, two_digit_year_cutoff))
  }

  if (is.character(x)) {
    dt <- moose_character_to_datetime(
      x,
      day_first = day_first,
      numeric_origin = numeric_origin,
      tz = tz,
      two_digit_year_cutoff = two_digit_year_cutoff
    )
    result <- as.Date(dt, tz = tz)
    return(moose_normalize_date_year(result, two_digit_year_cutoff))
  }

  stop(
    "`x` must be a Date, POSIXt, character, factor, integer, or numeric vector.",
    call. = FALSE
  )
}

#' @rdname Moose_todate
#' @export
Moose_todatetime <- function(x,
                             day_first = FALSE,
                             numeric_origin = c("auto", "excel", "unix", "r"),
                             tz = "UTC",
                             two_digit_year_cutoff = 68L) {
  numeric_origin <- match.arg(numeric_origin)
  day_first <- moose_validate_single_logical(day_first, "day_first")
  tz <- moose_validate_tz(tz)
  two_digit_year_cutoff <- moose_validate_two_digit_year_cutoff(
    two_digit_year_cutoff
  )

  if (is.null(x)) {
    return(as.POSIXct(character(), tz = tz))
  }

  if (inherits(x, "POSIXt")) {
    return(moose_normalize_datetime_year(x, two_digit_year_cutoff, tz))
  }

  if (inherits(x, "Date")) {
    normalized <- moose_normalize_date_year(x, two_digit_year_cutoff)
    return(as.POSIXct(normalized, tz = tz))
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.numeric(x) || is.integer(x)) {
    result <- moose_numeric_to_datetime(
      x,
      numeric_origin = numeric_origin,
      tz = tz,
      two_digit_year_cutoff = two_digit_year_cutoff
    )
    return(moose_normalize_datetime_year(
      result,
      two_digit_year_cutoff,
      tz
    ))
  }

  if (is.character(x)) {
    result <- moose_character_to_datetime(
      x,
      day_first = day_first,
      numeric_origin = numeric_origin,
      tz = tz,
      two_digit_year_cutoff = two_digit_year_cutoff
    )
    return(moose_normalize_datetime_year(
      result,
      two_digit_year_cutoff,
      tz
    ))
  }

  stop(
    "`x` must be a Date, POSIXt, character, factor, integer, or numeric vector.",
    call. = FALSE
  )
}

moose_character_to_datetime <- function(x,
                                        day_first,
                                        numeric_origin,
                                        tz,
                                        two_digit_year_cutoff = 68L) {
  values <- trimws(as.character(x))
  missing <- is.na(values) |
    !nzchar(values) |
    tolower(values) %in% c("na", "n/a", "null", "none")

  parse_values <- moose_normalize_datetime_text(values)
  out <- moose_posix_na(length(values), tz)

  inferred_day_first <- moose_infer_day_first(parse_values, day_first)
  formats <- moose_datetime_formats(inferred_day_first)
  out <- moose_parse_datetime_formats(
    parse_values,
    formats,
    tz,
    out,
    missing,
    two_digit_year_cutoff = two_digit_year_cutoff
  )

  remaining <- is.na(out) & !missing
  numeric_text <- remaining & grepl("^[+-]?[0-9]+([.][0-9]+)?$", values)

  if (any(numeric_text)) {
    numeric_values <- suppressWarnings(as.numeric(values[numeric_text]))
    out[numeric_text] <- moose_numeric_to_datetime(
      numeric_values,
      numeric_origin = numeric_origin,
      tz = tz,
      two_digit_year_cutoff = two_digit_year_cutoff
    )
  }

  out
}

moose_numeric_to_date <- function(x,
                                  numeric_origin,
                                  tz,
                                  two_digit_year_cutoff = 68L) {
  as.Date(moose_numeric_to_datetime(
    x,
    numeric_origin = numeric_origin,
    tz = tz,
    two_digit_year_cutoff = two_digit_year_cutoff
  ), tz = tz)
}

moose_numeric_to_datetime <- function(x,
                                      numeric_origin,
                                      tz,
                                      two_digit_year_cutoff = 68L) {
  out <- moose_posix_na(length(x), tz)
  finite <- !is.na(x) & is.finite(x)

  if (!any(finite)) {
    return(out)
  }

  if (identical(numeric_origin, "excel")) {
    out[finite] <- moose_excel_to_datetime(x[finite], tz)
    return(out)
  }

  if (identical(numeric_origin, "unix")) {
    out[finite] <- as.POSIXct(x[finite], origin = "1970-01-01", tz = tz)
    return(out)
  }

  if (identical(numeric_origin, "r")) {
    out[finite] <- as.POSIXct(x[finite] * 86400, origin = "1970-01-01", tz = tz)
    return(out)
  }

  compact <- finite & moose_is_whole_number(x)
  compact_text <- rep(NA_character_, length(x))
  compact_text[compact] <- format(
    round(abs(x[compact])),
    scientific = FALSE,
    trim = TRUE
  )

  compact_candidate <- compact &
    nchar(compact_text) %in% c(6L, 8L, 12L, 14L)

  if (any(compact_candidate)) {
    compact_dt <- moose_parse_compact_datetime(
      compact_text[compact_candidate],
      tz,
      two_digit_year_cutoff = two_digit_year_cutoff
    )
    good <- !is.na(compact_dt)
    idx <- which(compact_candidate)
    out[idx[good]] <- compact_dt[good]
  }

  remaining <- is.na(out) & finite

  unix_candidate <- remaining & x >= 100000000 & x <= 4102444800
  if (any(unix_candidate)) {
    out[unix_candidate] <- as.POSIXct(
      x[unix_candidate],
      origin = "1970-01-01",
      tz = tz
    )
  }

  remaining <- is.na(out) & finite
  excel_candidate <- remaining & x >= 30000 & x <= 60000
  if (any(excel_candidate)) {
    out[excel_candidate] <- moose_excel_to_datetime(x[excel_candidate], tz)
  }

  remaining <- is.na(out) & finite
  r_candidate <- remaining & x > -100000 & x < 30000
  if (any(r_candidate)) {
    out[r_candidate] <- as.POSIXct(
      x[r_candidate] * 86400,
      origin = "1970-01-01",
      tz = tz
    )
  }

  out
}

moose_parse_compact_datetime <- function(x,
                                         tz,
                                         two_digit_year_cutoff = 68L) {
  out <- moose_posix_na(length(x), tz)
  formats <- c(
    "%Y%m%d%H%M%S",
    "%Y%m%d%H%M",
    "%Y%m%d",
    "%y%m%d"
  )
  moose_parse_datetime_formats(
    x,
    formats,
    tz,
    out,
    rep(FALSE, length(x)),
    two_digit_year_cutoff = two_digit_year_cutoff
  )
}

moose_parse_datetime_formats <- function(x,
                                         formats,
                                         tz,
                                         out,
                                         missing,
                                         two_digit_year_cutoff = 68L) {
  for (fmt in formats) {
    needs_parse <- is.na(out) & !missing

    if (!any(needs_parse)) {
      break
    }

    needs_parse <- needs_parse & moose_candidate_matches_datetime_format(x, fmt)

    if (!any(needs_parse)) {
      next
    }

    parse_text <- x[needs_parse]
    parse_format <- fmt

    if (grepl("%y", fmt, fixed = TRUE)) {
      expanded <- moose_expand_two_digit_year(
        parse_text,
        fmt,
        two_digit_year_cutoff
      )
      parse_text <- expanded$text
      parse_format <- expanded$format
    }

    parsed <- suppressWarnings(strptime(
      parse_text,
      format = parse_format,
      tz = tz
    ))
    parsed <- as.POSIXct(parsed, tz = tz)
    good <- !is.na(parsed)

    if (any(good)) {
      idx <- which(needs_parse)
      out[idx[good]] <- parsed[good]
    }
  }

  out
}

moose_expand_two_digit_year <- function(x, fmt, cutoff) {
  if (startsWith(fmt, "%y%m%d")) {
    year_start <- rep(1L, length(x))
  } else {
    year_start <- regexpr(
      "[0-9]{2}(?=$|[T[:space:]][0-9]{1,2}:[0-9]{2})",
      x,
      perl = TRUE
    )
  }

  short_year <- as.integer(substr(x, year_start, year_start + 1L))
  full_year <- ifelse(
    short_year <= cutoff,
    2000L + short_year,
    1900L + short_year
  )

  expanded <- x
  for (i in seq_along(expanded)) {
    expanded[[i]] <- paste0(
      substr(x[[i]], 1L, year_start[[i]] - 1L),
      sprintf("%04d", full_year[[i]]),
      substr(x[[i]], year_start[[i]] + 2L, nchar(x[[i]]))
    )
  }

  list(
    text = expanded,
    format = sub("%y", "%Y", fmt, fixed = TRUE)
  )
}

moose_candidate_matches_datetime_format <- function(x, fmt) {
  if (identical(fmt, "%Y%m%d%H%M%S")) {
    return(grepl("^[0-9]{14}$", x))
  }

  if (identical(fmt, "%Y%m%d%H%M")) {
    return(grepl("^[0-9]{12}$", x))
  }

  if (identical(fmt, "%Y%m%d")) {
    return(grepl("^[0-9]{8}$", x))
  }

  if (identical(fmt, "%y%m%d")) {
    return(grepl("^[0-9]{6}$", x))
  }

  date_regexes <- c(
    "%Y-%m-%d" = "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}(?=$|[T[:space:]])",
    "%Y/%m/%d" = "^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}(?=$|[T[:space:]])",
    "%Y.%m.%d" = "^[0-9]{4}[.][0-9]{1,2}[.][0-9]{1,2}(?=$|[T[:space:]])",
    "%Y%m%d" = "^[0-9]{8}(?=$|[T[:space:]])",
    "%y%m%d" = "^[0-9]{6}(?=$|[T[:space:]])",
    "%m/%d/%Y" = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}(?=$|[T[:space:]])",
    "%m-%d-%Y" = "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}(?=$|[T[:space:]])",
    "%m.%d.%Y" = "^[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{4}(?=$|[T[:space:]])",
    "%d/%m/%Y" = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}(?=$|[T[:space:]])",
    "%d-%m-%Y" = "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}(?=$|[T[:space:]])",
    "%d.%m.%Y" = "^[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{4}(?=$|[T[:space:]])",
    "%m/%d/%y" = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}(?=$|[T[:space:]])",
    "%m-%d-%y" = "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2}(?=$|[T[:space:]])",
    "%m.%d.%y" = "^[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{2}(?=$|[T[:space:]])",
    "%d/%m/%y" = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}(?=$|[T[:space:]])",
    "%d-%m-%y" = "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2}(?=$|[T[:space:]])",
    "%d.%m.%y" = "^[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{2}(?=$|[T[:space:]])",
    "%d-%b-%Y" = "^[0-9]{1,2}-[[:alpha:]]+-[0-9]{4}(?=$|[T[:space:]])",
    "%d %b %Y" = "^[0-9]{1,2}[[:space:]]+[[:alpha:]]+[[:space:]]+[0-9]{4}(?=$|[T[:space:]])",
    "%d-%B-%Y" = "^[0-9]{1,2}-[[:alpha:]]+-[0-9]{4}(?=$|[T[:space:]])",
    "%d %B %Y" = "^[0-9]{1,2}[[:space:]]+[[:alpha:]]+[[:space:]]+[0-9]{4}(?=$|[T[:space:]])",
    "%b %d, %Y" = "^[[:alpha:]]+[[:space:]]+[0-9]{1,2},[[:space:]]*[0-9]{4}(?=$|[T[:space:]])",
    "%B %d, %Y" = "^[[:alpha:]]+[[:space:]]+[0-9]{1,2},[[:space:]]*[0-9]{4}(?=$|[T[:space:]])",
    "%d-%b-%y" = "^[0-9]{1,2}-[[:alpha:]]+-[0-9]{2}(?=$|[T[:space:]])",
    "%d %b %y" = "^[0-9]{1,2}[[:space:]]+[[:alpha:]]+[[:space:]]+[0-9]{2}(?=$|[T[:space:]])",
    "%d-%B-%y" = "^[0-9]{1,2}-[[:alpha:]]+-[0-9]{2}(?=$|[T[:space:]])",
    "%d %B %y" = "^[0-9]{1,2}[[:space:]]+[[:alpha:]]+[[:space:]]+[0-9]{2}(?=$|[T[:space:]])",
    "%b %d, %y" = "^[[:alpha:]]+[[:space:]]+[0-9]{1,2},[[:space:]]*[0-9]{2}(?=$|[T[:space:]])",
    "%B %d, %y" = "^[[:alpha:]]+[[:space:]]+[0-9]{1,2},[[:space:]]*[0-9]{2}(?=$|[T[:space:]])"
  )
  matching_format <- startsWith(fmt, names(date_regexes))

  if (any(matching_format)) {
    format_prefix <- names(date_regexes)[which(matching_format)[1L]]
    date_regex <- unname(date_regexes[[format_prefix]])
    suffix <- substring(fmt, nchar(format_prefix) + 1L)
    suffix_regex <- moose_datetime_suffix_regex(suffix)

    if (!is.na(suffix_regex)) {
      return(grepl(
        paste0(date_regex, suffix_regex),
        x,
        perl = TRUE
      ))
    }
  }

  rep(FALSE, length(x))
}

moose_datetime_suffix_regex <- function(suffix) {
  if (!nzchar(suffix)) {
    return("$")
  }

  has_offset <- endsWith(suffix, "%z")
  if (has_offset) {
    suffix <- substr(suffix, 1L, nchar(suffix) - 2L)
  }

  if (startsWith(suffix, "T")) {
    separator <- "T"
    time_format <- substring(suffix, 2L)
  } else if (startsWith(suffix, " ")) {
    separator <- "[[:space:]]+"
    time_format <- substring(suffix, 2L)
  } else {
    return(NA_character_)
  }

  time_regex <- switch(
    time_format,
    "%H:%M:%OS" = "[0-9]{1,2}:[0-9]{2}:[0-9]{2}(?:[.][0-9]+)?",
    "%H:%M:%S" = "[0-9]{1,2}:[0-9]{2}:[0-9]{2}",
    "%H:%M" = "[0-9]{1,2}:[0-9]{2}",
    "%I:%M:%OS %p" = paste0(
      "[0-9]{1,2}:[0-9]{2}:[0-9]{2}(?:[.][0-9]+)?",
      "[[:space:]]+[AaPp][Mm]"
    ),
    "%I:%M:%S %p" = paste0(
      "[0-9]{1,2}:[0-9]{2}:[0-9]{2}",
      "[[:space:]]+[AaPp][Mm]"
    ),
    "%I:%M %p" = paste0(
      "[0-9]{1,2}:[0-9]{2}[[:space:]]+[AaPp][Mm]"
    ),
    NULL
  )

  if (is.null(time_regex)) {
    return(NA_character_)
  }

  offset_regex <- if (has_offset) "[+-][0-9]{4}" else ""
  paste0(separator, time_regex, offset_regex, "$")
}

moose_datetime_formats <- function(day_first) {
  date_formats <- moose_date_formats(day_first)
  time_formats <- c(
    "%H:%M:%OS",
    "%H:%M:%S",
    "%H:%M",
    "%I:%M:%OS %p",
    "%I:%M:%S %p",
    "%I:%M %p"
  )

  datetime_formats <- as.vector(outer(date_formats, time_formats, paste))
  iso_formats <- as.vector(outer(
    c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d"),
    time_formats,
    paste,
    sep = "T"
  ))
  offset_formats <- paste0(c(datetime_formats, iso_formats), "%z")

  unique(c(offset_formats, datetime_formats, iso_formats, date_formats))
}

moose_date_formats <- function(day_first) {
  ymd <- c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d")
  ymd_short <- "%y%m%d"
  month_names <- c(
    "%d-%b-%Y", "%d %b %Y", "%d-%B-%Y", "%d %B %Y",
    "%b %d, %Y", "%B %d, %Y",
    "%d-%b-%y", "%d %b %y", "%d-%B-%y", "%d %B %y",
    "%b %d, %y", "%B %d, %y"
  )

  if (isTRUE(day_first)) {
    ambiguous <- c(
      "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
      "%m/%d/%Y", "%m-%d-%Y", "%m.%d.%Y",
      "%d/%m/%y", "%d-%m-%y", "%d.%m.%y",
      "%m/%d/%y", "%m-%d-%y", "%m.%d.%y"
    )
  } else {
    ambiguous <- c(
      "%m/%d/%Y", "%m-%d-%Y", "%m.%d.%Y",
      "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
      "%m/%d/%y", "%m-%d-%y", "%m.%d.%y",
      "%d/%m/%y", "%d-%m-%y", "%d.%m.%y"
    )
  }

  c(ymd, ymd_short, ambiguous, month_names)
}

moose_infer_day_first <- function(x, fallback) {
  candidate <- !is.na(x) & grepl(
    paste0(
      "^[0-9]{1,2}([-/.])[0-9]{1,2}\\1",
      "[0-9]{2}(?:[0-9]{2})?(?=$|[T[:space:]])"
    ),
    x,
    perl = TRUE
  )

  if (!any(candidate)) {
    return(fallback)
  }

  values <- x[candidate]
  first <- as.integer(sub(
    "^([0-9]{1,2}).*$",
    "\\1",
    values,
    perl = TRUE
  ))
  second <- as.integer(sub(
    "^[0-9]{1,2}[-/.]([0-9]{1,2}).*$",
    "\\1",
    values,
    perl = TRUE
  ))

  day_first_evidence <- sum(
    first >= 13L & first <= 31L & second >= 1L & second <= 12L
  )
  month_first_evidence <- sum(
    second >= 13L & second <= 31L & first >= 1L & first <= 12L
  )

  if (day_first_evidence > month_first_evidence) {
    return(TRUE)
  }

  if (month_first_evidence > day_first_evidence) {
    return(FALSE)
  }

  fallback
}

moose_normalize_date_year <- function(x, cutoff) {
  if (!length(x)) {
    return(x)
  }

  year <- suppressWarnings(as.integer(format(x, "%Y")))
  needs_normalization <- !is.na(year) & year >= 0L & year <= 99L

  if (!any(needs_normalization)) {
    return(x)
  }

  full_year <- ifelse(
    year[needs_normalization] <= cutoff,
    2000L + year[needs_normalization],
    1900L + year[needs_normalization]
  )
  replacement <- paste0(
    sprintf("%04d", full_year),
    format(x[needs_normalization], "-%m-%d")
  )
  x[needs_normalization] <- as.Date(replacement, format = "%Y-%m-%d")
  x
}

moose_normalize_datetime_year <- function(x, cutoff, tz) {
  if (!length(x)) {
    return(as.POSIXct(x, tz = tz))
  }

  parts <- as.POSIXlt(x, tz = tz)
  year <- parts$year + 1900L
  needs_normalization <- !is.na(year) & year >= 0L & year <= 99L

  if (!any(needs_normalization)) {
    return(as.POSIXct(parts, tz = tz))
  }

  full_year <- ifelse(
    year[needs_normalization] <= cutoff,
    2000L + year[needs_normalization],
    1900L + year[needs_normalization]
  )
  parts$year[needs_normalization] <- full_year - 1900L
  as.POSIXct(parts, tz = tz)
}

moose_normalize_datetime_text <- function(x) {
  has_time <- !is.na(x) & grepl(
    "[T[:space:]][0-9]{1,2}:[0-9]{2}",
    x,
    perl = TRUE
  )
  x[has_time] <- sub("Z$", "+0000", x[has_time])
  x[has_time] <- sub(
    "([+-][0-9]{2}):([0-9]{2})$",
    "\\1\\2",
    x[has_time],
    perl = TRUE
  )
  trimws(x)
}

moose_excel_to_datetime <- function(x, tz) {
  as.POSIXct((x - 25569) * 86400, origin = "1970-01-01", tz = tz)
}

moose_is_whole_number <- function(x) {
  abs(x - round(x)) < sqrt(.Machine$double.eps)
}

moose_posix_na <- function(n, tz) {
  as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = tz)
}

moose_validate_single_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  x
}

moose_validate_two_digit_year_cutoff <- function(x) {
  valid <- is.numeric(x) &&
    !is.logical(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x >= 0 &&
    x <= 99 &&
    x == floor(x)

  if (!valid) {
    stop(
      "`two_digit_year_cutoff` must be one whole number from 0 to 99.",
      call. = FALSE
    )
  }

  as.integer(x)
}

moose_validate_tz <- function(tz) {
  if (!is.character(tz) || length(tz) != 1L || is.na(tz) || !nzchar(tz)) {
    stop("`tz` must be one non-missing character value.", call. = FALSE)
  }

  tz
}
