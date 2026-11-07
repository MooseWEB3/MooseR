# Shared argument checks for the routing and travel functions. Each stops with
# a message naming the argument, and returns the validated value invisibly.

moose_check_text <- function(x, argument) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("`", argument, "` must be one non-empty character value.", call. = FALSE)
  }
  invisible(x)
}

moose_check_flag <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", argument, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

# A single finite number of at least `minimum` (or above it when
# `inclusive = FALSE`).
moose_check_number <- function(x, argument, minimum, inclusive = TRUE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      (if (inclusive) x < minimum else x <= minimum)) {
    stop("`", argument, "` must be a finite number ",
         if (inclusive) ">= " else "> ", minimum, ".", call. = FALSE)
  }
  invisible(x)
}

# Coordinates from a data column: numbers, numeric strings or factors. Values
# that are not numbers become NA.
moose_as_coordinate <- function(x) {
  if (!is.atomic(x) || !is.null(dim(x))) {
    stop("Coordinate columns must be atomic vectors.", call. = FALSE)
  }
  suppressWarnings(as.numeric(as.character(x)))
}

# TRUE where longitude and latitude are finite decimal degrees in range.
moose_valid_lonlat <- function(lon, lat) {
  is.finite(lon) & abs(lon) <= 180 & is.finite(lat) & abs(lat) <= 90
}
