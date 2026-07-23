#' Automatically convert date-like columns in a data set
#'
#' Inspects each column of a data frame and converts columns that strongly
#' resemble dates or date-times. Existing `Date` and `POSIXt` columns are left
#' unchanged. Character and factor columns are recognized from their values;
#' numeric columns also require a date-related column name to reduce false
#' positives.
#'
#' @param data A data frame or data-frame subclass.
#' @param day_first Logical. If `TRUE`, ambiguous dates such as `01/02/2024`
#'   are interpreted as day/month/year before month/day/year.
#' @param numeric_origin Character. One of `"auto"`, `"excel"`, `"unix"`, or
#'   `"r"`. Passed to [Moose_todate()] and [Moose_todatetime()].
#' @param tz Time zone used when parsing date-time columns.
#' @param min_success Numeric value between 0 and 1. At least this proportion
#'   of non-missing values must look like dates and convert successfully before
#'   a column is changed.
#' @param verbose Logical. If `TRUE`, report which columns were converted.
#'
#' @return A copy of `data` with confidently detected date columns converted
#'   to `Date` and date-time columns converted to `POSIXct`.
#'
#' @examples
#' raw_data <- data.frame(
#'   visit_date = c("2024-01-05", "2024/01/06"),
#'   created_at = c("2024-01-05 13:30", "2024-01-06 08:05"),
#'   note = c("First visit", "Follow-up")
#' )
#'
#' boosted <- Moose_boost_data(raw_data)
#' class(boosted$visit_date)
#' class(boosted$created_at)
#'
#' @export
Moose_boost_data <- function(data,
                             day_first = FALSE,
                             numeric_origin = c("auto", "excel", "unix", "r"),
                             tz = "UTC",
                             min_success = 0.8,
                             verbose = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or data-frame subclass.", call. = FALSE)
  }

  numeric_origin <- match.arg(numeric_origin)
  day_first <- moose_validate_single_logical(day_first, "day_first")
  tz <- moose_validate_tz(tz)
  verbose <- moose_validate_single_logical(verbose, "verbose")

  if (!is.numeric(min_success) ||
      length(min_success) != 1L ||
      is.na(min_success) ||
      !is.finite(min_success) ||
      min_success <= 0 ||
      min_success > 1) {
    stop("`min_success` must be one number greater than 0 and at most 1.",
         call. = FALSE)
  }

  output <- data
  converted <- character()

  for (column_name in names(output)) {
    column <- output[[column_name]]

    if (inherits(column, "Date") || inherits(column, "POSIXt")) {
      next
    }

    if (!is.character(column) &&
        !is.factor(column) &&
        !is.numeric(column) &&
        !is.integer(column)) {
      next
    }

    name_type <- moose_boost_name_type(column_name)
    detection <- moose_boost_detect_column(
      column,
      name_type = name_type,
      day_first = day_first,
      numeric_origin = numeric_origin,
      tz = tz,
      min_success = min_success
    )

    if (is.null(detection)) {
      next
    }

    output[[column_name]] <- detection$value
    converted <- c(
      converted,
      paste0(column_name, " (", detection$type, ")")
    )
  }

  if (isTRUE(verbose)) {
    if (length(converted)) {
      message("Moose_boost_data converted: ", paste(converted, collapse = ", "))
    } else {
      message("Moose_boost_data did not find any date-like columns.")
    }
  }

  output
}

moose_boost_detect_column <- function(column,
                                      name_type,
                                      day_first,
                                      numeric_origin,
                                      tz,
                                      min_success) {
  if (is.factor(column)) {
    column <- as.character(column)
  }

  if (is.character(column)) {
    meaningful <- moose_boost_meaningful_character(column)
    if (!any(meaningful)) {
      return(NULL)
    }

    shape_match <- moose_boost_character_shape(
      column[meaningful],
      name_type = name_type,
      numeric_origin = numeric_origin,
      tz = tz
    )

    if (mean(shape_match) < min_success) {
      return(NULL)
    }

    target_type <- if (
      identical(name_type, "POSIXct") ||
      any(moose_boost_has_time_component(column[meaningful]))
    ) {
      "POSIXct"
    } else {
      "Date"
    }
  } else {
    meaningful <- !is.na(column) & is.finite(column)
    if (!any(meaningful) || identical(name_type, "none")) {
      return(NULL)
    }

    shape_match <- moose_boost_numeric_shape(
      column[meaningful],
      name_type = name_type,
      numeric_origin = numeric_origin,
      tz = tz
    )

    if (mean(shape_match) < min_success) {
      return(NULL)
    }

    target_type <- name_type
  }

  converted <- if (identical(target_type, "POSIXct")) {
    Moose_todatetime(
      column,
      day_first = day_first,
      numeric_origin = numeric_origin,
      tz = tz
    )
  } else {
    Moose_todate(
      column,
      day_first = day_first,
      numeric_origin = numeric_origin,
      tz = tz
    )
  }

  if (mean(!is.na(converted[meaningful])) < min_success) {
    return(NULL)
  }

  list(value = converted, type = target_type)
}

moose_boost_name_type <- function(column_name) {
  snake_name <- gsub(
    "([[:lower:][:digit:]])([[:upper:]])",
    "\\1_\\2",
    column_name,
    perl = TRUE
  )
  normalized <- tolower(gsub("[^[:alnum:]]+", "_", snake_name))
  tokens <- strsplit(normalized, "_", fixed = TRUE)[[1L]]
  tokens <- tokens[nzchar(tokens)]

  if (any(tokens %in% c("datetime", "timestamp")) ||
      grepl("date_time", normalized, fixed = TRUE) ||
      any(tokens == "time") ||
      grepl("(^|_)at$", normalized)) {
    return("POSIXct")
  }

  if (any(tokens %in% c("date", "dob", "birthdate")) ||
      grepl("date_of_birth", normalized, fixed = TRUE) ||
      grepl("birth_date", normalized, fixed = TRUE)) {
    return("Date")
  }

  "none"
}

moose_boost_meaningful_character <- function(x) {
  values <- trimws(as.character(x))
  !is.na(values) &
    nzchar(values) &
    !tolower(values) %in% c("na", "n/a", "null", "none")
}

moose_boost_character_shape <- function(x, name_type, numeric_origin, tz) {
  values <- trimws(as.character(x))
  standard_date <- grepl(
    paste0(
      "^(?:",
      "[0-9]{4}[-/.][0-9]{1,2}[-/.][0-9]{1,2}",
      "|[0-9]{1,2}[-/.][0-9]{1,2}[-/.][0-9]{4}",
      "|[0-9]{8}(?:[T[:space:]]?[0-9]{4}(?:[0-9]{2})?)?",
      ")"
    ),
    values,
    perl = TRUE
  )
  month_name <- grepl(
    paste0(
      "^(?:",
      "[0-9]{1,2}[-[:space:]]",
      "|[[:alpha:]]{3,9}[[:space:]][0-9]{1,2},?[[:space:]]",
      ").*[0-9]{4}"
    ),
    values,
    perl = TRUE
  )

  numeric_text <- grepl("^[+-]?[0-9]+(?:[.][0-9]+)?$", values, perl = TRUE)
  numeric_shape <- rep(FALSE, length(values))

  if (!identical(name_type, "none") && any(numeric_text)) {
    numeric_values <- suppressWarnings(as.numeric(values[numeric_text]))
    numeric_shape[numeric_text] <- moose_boost_numeric_shape(
      numeric_values,
      name_type = name_type,
      numeric_origin = numeric_origin,
      tz = tz
    )
  }

  standard_date | month_name | numeric_shape
}

moose_boost_numeric_shape <- function(x, name_type, numeric_origin, tz) {
  finite <- !is.na(x) & is.finite(x)
  result <- rep(FALSE, length(x))

  if (!any(finite)) {
    return(result)
  }

  if (!identical(numeric_origin, "auto")) {
    result[finite] <- TRUE
    return(result)
  }

  whole <- finite & moose_is_whole_number(x)
  compact_text <- rep(NA_character_, length(x))
  compact_text[whole] <- format(
    round(abs(x[whole])),
    scientific = FALSE,
    trim = TRUE
  )
  compact <- whole & nchar(compact_text) %in% c(8L, 12L, 14L)

  if (any(compact)) {
    result[compact] <- !is.na(
      moose_parse_compact_datetime(compact_text[compact], tz)
    )
  }

  result <- result |
    (finite & x >= 100000000 & x <= 4102444800) |
    (finite & x >= 30000 & x <= 60000)

  if (identical(name_type, "Date")) {
    result <- result | (finite & x > -100000 & x < 30000)
  }

  result
}

moose_boost_has_time_component <- function(x) {
  values <- trimws(as.character(x))
  grepl(
    "(?:[T[:space:]][0-9]{1,2}:[0-9]{2}|^[0-9]{12}(?:[0-9]{2})?$)",
    values,
    perl = TRUE
  )
}
