#' Convert input to R Date or date-time values
#'
#' `Moose_todate()` converts common date inputs to `Date`.
#' `Moose_todatetime()` converts common date or date-time inputs to `POSIXct`.
#'
#' @param x Input vector. Supports `Date`, `POSIXt`, character, factor, integer,
#'   and numeric values.
#' @param day_first Logical. If `TRUE`, ambiguous dates such as `01/02/2024`
#'   are parsed as day/month/year before month/day/year.
#' @param numeric_origin Character. One of `"auto"`, `"excel"`, `"unix"`, or
#'   `"r"`. In `"auto"` mode, compact values such as `20240131` are parsed as
#'   `YYYYMMDD`, large values are parsed as Unix seconds, Excel-like serial
#'   values are parsed using Excel's Windows origin, and smaller values are
#'   parsed as R days since 1970-01-01.
#' @param tz Time zone used for date-time parsing and conversion.
#'
#' @return `Moose_todate()` returns a `Date` vector. `Moose_todatetime()`
#'   returns a `POSIXct` vector.
#'
#' @examples
#' Moose_todate(c("2024-01-05", "01/06/2024", "20240107"))
#' Moose_todate(c(20240105, 45296))
#'
#' Moose_todatetime(c("2024-01-05 13:30:00", "2024/01/06 8:05"))
#' Moose_todatetime(c(202401051330, 1704450600))
#'
#' @export
Moose_todate <- function(x,
                         day_first = FALSE,
                         numeric_origin = c("auto", "excel", "unix", "r"),
                         tz = "UTC") {
  numeric_origin <- match.arg(numeric_origin)
  day_first <- moose_validate_single_logical(day_first, "day_first")
  tz <- moose_validate_tz(tz)

  if (is.null(x)) {
    return(as.Date(character()))
  }

  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x, tz = tz))
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.numeric(x) || is.integer(x)) {
    return(moose_numeric_to_date(x, numeric_origin = numeric_origin, tz = tz))
  }

  if (is.character(x)) {
    dt <- moose_character_to_datetime(
      x,
      day_first = day_first,
      numeric_origin = numeric_origin,
      tz = tz
    )
    return(as.Date(dt, tz = tz))
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
                             tz = "UTC") {
  numeric_origin <- match.arg(numeric_origin)
  day_first <- moose_validate_single_logical(day_first, "day_first")
  tz <- moose_validate_tz(tz)

  if (is.null(x)) {
    return(as.POSIXct(character(), tz = tz))
  }

  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = tz))
  }

  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = tz))
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.numeric(x) || is.integer(x)) {
    return(moose_numeric_to_datetime(
      x,
      numeric_origin = numeric_origin,
      tz = tz
    ))
  }

  if (is.character(x)) {
    return(moose_character_to_datetime(
      x,
      day_first = day_first,
      numeric_origin = numeric_origin,
      tz = tz
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
                                        tz) {
  values <- trimws(as.character(x))
  missing <- is.na(values) |
    !nzchar(values) |
    tolower(values) %in% c("na", "n/a", "null", "none")

  parse_values <- moose_normalize_datetime_text(values)
  out <- moose_posix_na(length(values), tz)

  formats <- moose_datetime_formats(day_first)
  out <- moose_parse_datetime_formats(parse_values, formats, tz, out, missing)

  remaining <- is.na(out) & !missing
  numeric_text <- remaining & grepl("^[+-]?[0-9]+([.][0-9]+)?$", values)

  if (any(numeric_text)) {
    numeric_values <- suppressWarnings(as.numeric(values[numeric_text]))
    out[numeric_text] <- moose_numeric_to_datetime(
      numeric_values,
      numeric_origin = numeric_origin,
      tz = tz
    )
  }

  out
}

moose_numeric_to_date <- function(x, numeric_origin, tz) {
  as.Date(moose_numeric_to_datetime(
    x,
    numeric_origin = numeric_origin,
    tz = tz
  ), tz = tz)
}

moose_numeric_to_datetime <- function(x, numeric_origin, tz) {
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
    nchar(compact_text) %in% c(8L, 12L, 14L)

  if (any(compact_candidate)) {
    compact_dt <- moose_parse_compact_datetime(compact_text[compact_candidate], tz)
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

moose_parse_compact_datetime <- function(x, tz) {
  out <- moose_posix_na(length(x), tz)
  formats <- c("%Y%m%d%H%M%S", "%Y%m%d%H%M", "%Y%m%d")
  moose_parse_datetime_formats(x, formats, tz, out, rep(FALSE, length(x)))
}

moose_parse_datetime_formats <- function(x, formats, tz, out, missing) {
  for (fmt in formats) {
    needs_parse <- is.na(out) & !missing

    if (!any(needs_parse)) {
      break
    }

    needs_parse <- needs_parse & moose_candidate_matches_datetime_format(x, fmt)

    if (!any(needs_parse)) {
      next
    }

    parsed <- suppressWarnings(strptime(x[needs_parse], format = fmt, tz = tz))
    parsed <- as.POSIXct(parsed, tz = tz)
    good <- !is.na(parsed)

    if (any(good)) {
      idx <- which(needs_parse)
      out[idx[good]] <- parsed[good]
    }
  }

  out
}

moose_candidate_matches_datetime_format <- function(x, fmt) {
  if (!grepl("%Y%m%d", fmt, fixed = TRUE)) {
    if (startsWith(fmt, "%Y-%m-%d")) {
      return(grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", x))
    }

    if (startsWith(fmt, "%Y/%m/%d")) {
      return(grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}", x))
    }

    if (startsWith(fmt, "%Y.%m.%d")) {
      return(grepl("^[0-9]{4}[.][0-9]{1,2}[.][0-9]{1,2}", x))
    }

    if (startsWith(fmt, "%m/%d/%Y")) {
      return(grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x))
    }

    if (startsWith(fmt, "%m-%d-%Y")) {
      return(grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}", x))
    }

    if (startsWith(fmt, "%m.%d.%Y")) {
      return(grepl("^[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{4}", x))
    }

    if (startsWith(fmt, "%d/%m/%Y")) {
      return(grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x))
    }

    if (startsWith(fmt, "%d-%m-%Y")) {
      return(grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}", x))
    }

    if (startsWith(fmt, "%d.%m.%Y")) {
      return(grepl("^[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{4}", x))
    }

    return(rep(TRUE, length(x)))
  }

  if (identical(fmt, "%Y%m%d%H%M%S")) {
    return(grepl("^[0-9]{14}$", x))
  }

  if (identical(fmt, "%Y%m%d%H%M")) {
    return(grepl("^[0-9]{12}$", x))
  }

  if (identical(fmt, "%Y%m%d")) {
    return(grepl("^[0-9]{8}$", x))
  }

  if (grepl("T", fmt, fixed = TRUE)) {
    return(grepl("^[0-9]{8}T", x))
  }

  if (grepl(" ", fmt, fixed = TRUE)) {
    return(grepl("^[0-9]{8}[[:space:]]", x))
  }

  rep(TRUE, length(x))
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

  unique(c(datetime_formats, iso_formats, date_formats))
}

moose_date_formats <- function(day_first) {
  ymd <- c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d")
  month_names <- c("%d-%b-%Y", "%d %b %Y", "%d-%B-%Y", "%d %B %Y")
  month_names <- c(month_names, "%b %d, %Y", "%B %d, %Y")

  if (isTRUE(day_first)) {
    ambiguous <- c(
      "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
      "%m/%d/%Y", "%m-%d-%Y", "%m.%d.%Y"
    )
  } else {
    ambiguous <- c(
      "%m/%d/%Y", "%m-%d-%Y", "%m.%d.%Y",
      "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y"
    )
  }

  c(ymd, ambiguous, month_names)
}

moose_normalize_datetime_text <- function(x) {
  x <- sub("Z$", "", x)
  x <- sub("([+-][0-9]{2}:?[0-9]{2})$", "", x)
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

moose_validate_tz <- function(tz) {
  if (!is.character(tz) || length(tz) != 1L || is.na(tz) || !nzchar(tz)) {
    stop("`tz` must be one non-missing character value.", call. = FALSE)
  }

  tz
}
