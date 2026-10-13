#' Format seconds as an elapsed duration
#'
#' Converts durations in seconds to `HH:MM:SS` strings. Hours are cumulative,
#' so durations do not wrap at 24 hours. Each component has at least two digits;
#' hours can have more than two digits for long durations.
#'
#' Fractional seconds are rounded to the nearest whole second, with half
#' seconds rounded up, before splitting into hours, minutes and seconds.
#' This is an elapsed duration, independent of dates and time zones.
#'
#' @param seconds A numeric vector of non-negative, finite seconds. `NA` and
#'   `NaN` are allowed and return `NA_character_`. A logical vector containing
#'   only `NA` is also accepted. Negative values, infinite values, character
#'   input, matrices and classed objects are rejected.
#' @return A character vector with the same length, order and names as `seconds`.
#' @examples
#' Moose_second2format(1000) # "00:16:40"
#' Moose_second2format(c(0, 60, 3600, 90061, NA))
#' Moose_second2format(59.5) # "00:01:00"
#'
#' trips <- data.frame(travel_time_seconds = c(1000, 3661, NA))
#' trips$travel_time_formatted <- Moose_second2format(trips$travel_time_seconds)
#' @export
Moose_second2format <- function(seconds) {
  numeric_input <- is.numeric(seconds) && !is.complex(seconds)
  missing_logical <- is.logical(seconds) && all(is.na(seconds))
  if (!(numeric_input || missing_logical) || is.object(seconds) ||
      !is.null(dim(seconds))) {
    stop("`seconds` must be a numeric vector of seconds.", call. = FALSE)
  }

  missing <- is.na(seconds)
  values <- seconds[!missing]
  if (any(!is.finite(values)) || any(values < 0)) {
    stop("Non-missing `seconds` must be finite and non-negative.", call. = FALSE)
  }

  # Round before splitting so 59.5 seconds carries into the next minute.
  whole <- floor(values)
  whole <- whole + (values - whole >= 0.5)
  result <- rep(NA_character_, length(seconds))
  names(result) <- names(seconds)
  result[!missing] <- sprintf(
    "%02.0f:%02.0f:%02.0f",
    whole %/% 3600, (whole %% 3600) %/% 60, whole %% 60
  )
  result
}
