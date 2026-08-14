#' Add road travel distance and time to a data set
#'
#' Looks up a road route for each unique origin-destination coordinate pair
#' using an OSRM-compatible routing server. The returned data frame contains
#' the original columns plus distance in metres and travel time in seconds.
#'
#' The public OSRM demonstration server is the default and is suitable for
#' light, interactive use. For large or production workloads, supply the URL
#' of an OSRM server you operate or are authorized to use.
#'
#' @param dataset A data frame or tibble.
#' @param start_latitude,start_longitude Column names containing origin
#'   latitude and longitude.
#' @param end_latitude,end_longitude Column names containing destination
#'   latitude and longitude.
#' @param profile OSRM routing profile, usually `"driving"`, `"walking"`, or
#'   `"cycling"`, when supported by the selected server.
#' @param server Base URL of an OSRM-compatible server.
#' @param username,password Optional HTTP Basic Authentication credentials.
#'   Supply both or neither. Prefer environment variables instead of writing a
#'   password directly in an R script.
#' @param distance_column Name of the output distance column, in metres.
#' @param time_column Name of the output travel-time column, in seconds.
#' @param overwrite Logical. Allow existing output columns to be replaced.
#' @param on_error One of `"warn"`, `"stop"`, or `"na"`. Controls request and
#'   no-route failures. Invalid or missing coordinates always return `NA`.
#' @param timeout Positive number of seconds used as the R connection timeout.
#' @param delay Non-negative seconds to pause between unique route requests.
#' @param progress Logical. Print request progress.
#'
#' @return The input data frame with two numeric columns appended.
#'
#' @examples
#' \dontrun{
#' trips <- data.frame(
#'   start_lat = c(53.5461, 51.0447),
#'   start_lon = c(-113.4938, -114.0719),
#'   end_lat = c(53.5444, 51.0486),
#'   end_lon = c(-113.4909, -114.0708)
#' )
#'
#' Moose_travel(
#'   trips,
#'   start_latitude = "start_lat",
#'   start_longitude = "start_lon",
#'   end_latitude = "end_lat",
#'   end_longitude = "end_lon"
#' )
#' }
#'
#' @export
Moose_travel <- function(dataset,
                         start_latitude,
                         start_longitude,
                         end_latitude,
                         end_longitude,
                         profile = "driving",
                         server = "https://router.project-osrm.org",
                         username = NULL,
                         password = NULL,
                         distance_column = "travel_distance_meters",
                         time_column = "travel_time_seconds",
                         overwrite = FALSE,
                         on_error = c("warn", "stop", "na"),
                         timeout = 60,
                         delay = 0.1,
                         progress = interactive()) {
  moose_travel_impl(
    dataset = dataset,
    start_latitude = start_latitude,
    start_longitude = start_longitude,
    end_latitude = end_latitude,
    end_longitude = end_longitude,
    profile = profile,
    server = server,
    username = username,
    password = password,
    distance_column = distance_column,
    time_column = time_column,
    overwrite = overwrite,
    on_error = on_error,
    timeout = timeout,
    delay = delay,
    progress = progress,
    route_fun = moose_osrm_route
  )
}

moose_travel_impl <- function(dataset,
                              start_latitude,
                              start_longitude,
                              end_latitude,
                              end_longitude,
                              profile,
                              server,
                              username,
                              password,
                              distance_column,
                              time_column,
                              overwrite,
                              on_error,
                              timeout,
                              delay,
                              progress,
                              route_fun) {
  if (!is.data.frame(dataset)) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }

  coordinate_columns <- c(
    start_latitude = start_latitude,
    start_longitude = start_longitude,
    end_latitude = end_latitude,
    end_longitude = end_longitude
  )
  valid_names <- vapply(
    coordinate_columns,
    function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x),
    logical(1)
  )
  if (!all(valid_names)) {
    stop("Coordinate arguments must each be one non-empty column name.", call. = FALSE)
  }
  missing_columns <- setdiff(unname(coordinate_columns), names(dataset))
  if (length(missing_columns)) {
    stop("Unknown coordinate columns: ", paste(missing_columns, collapse = ", "), ".", call. = FALSE)
  }

  validate_text <- function(x, argument) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      stop("`", argument, "` must be one non-empty character value.", call. = FALSE)
    }
    trimws(x)
  }
  profile <- validate_text(profile, "profile")
  server <- sub("/+$", "", validate_text(server, "server"))
  credentials_supplied <- c(!is.null(username), !is.null(password))
  if (any(credentials_supplied) && !all(credentials_supplied)) {
    stop("`username` and `password` must be supplied together.", call. = FALSE)
  }
  if (all(credentials_supplied)) {
    username <- validate_text(username, "username")
    password <- validate_text(password, "password")
  }
  distance_column <- validate_text(distance_column, "distance_column")
  time_column <- validate_text(time_column, "time_column")
  if (identical(distance_column, time_column)) {
    stop("`distance_column` and `time_column` must be different.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  conflicts <- intersect(c(distance_column, time_column), names(dataset))
  if (length(conflicts) && !overwrite) {
    stop("Output columns already exist: ", paste(conflicts, collapse = ", "),
         ". Use `overwrite = TRUE` to replace them.", call. = FALSE)
  }
  on_error <- match.arg(on_error, c("warn", "stop", "na"))
  validate_number <- function(x, argument, allow_zero) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
        x < if (allow_zero) 0 else .Machine$double.eps) {
      stop("`", argument, "` must be ", if (allow_zero) "non-negative." else "positive.", call. = FALSE)
    }
    as.numeric(x)
  }
  timeout <- validate_number(timeout, "timeout", FALSE)
  delay <- validate_number(delay, "delay", TRUE)
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("`progress` must be TRUE or FALSE.", call. = FALSE)
  }

  coords <- lapply(unname(coordinate_columns), function(column) {
    suppressWarnings(as.numeric(as.character(dataset[[column]])))
  })
  names(coords) <- names(coordinate_columns)
  valid <- is.finite(coords$start_latitude) &
    is.finite(coords$start_longitude) &
    is.finite(coords$end_latitude) &
    is.finite(coords$end_longitude) &
    abs(coords$start_latitude) <= 90 &
    abs(coords$end_latitude) <= 90 &
    abs(coords$start_longitude) <= 180 &
    abs(coords$end_longitude) <= 180

  distance <- rep(NA_real_, nrow(dataset))
  duration <- rep(NA_real_, nrow(dataset))
  valid_rows <- which(valid)

  if (length(valid_rows)) {
    keys <- sprintf(
      "%.8f|%.8f|%.8f|%.8f",
      coords$start_latitude[valid_rows], coords$start_longitude[valid_rows],
      coords$end_latitude[valid_rows], coords$end_longitude[valid_rows]
    )
    unique_positions <- which(!duplicated(keys))
    unique_rows <- valid_rows[unique_positions]
    unique_keys <- keys[unique_positions]
    results <- vector("list", length(unique_rows))
    failures <- character()

    old_timeout <- getOption("timeout")
    new_timeout <- if (is.null(old_timeout)) timeout else max(old_timeout, timeout)
    options(timeout = new_timeout)
    on.exit(options(timeout = old_timeout), add = TRUE)

    for (i in seq_along(unique_rows)) {
      row_id <- unique_rows[i]
      if (progress) message("Requesting route ", i, " of ", length(unique_rows), "...")
      result <- tryCatch(
        route_fun(
          start_lon = coords$start_longitude[row_id],
          start_lat = coords$start_latitude[row_id],
          end_lon = coords$end_longitude[row_id],
          end_lat = coords$end_latitude[row_id],
          profile = profile,
          server = server,
          username = username,
          password = password
        ),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        message_text <- paste0("Row ", row_id, ": ", conditionMessage(result))
        if (on_error == "stop") stop(message_text, call. = FALSE)
        failures <- c(failures, message_text)
        results[[i]] <- c(distance = NA_real_, duration = NA_real_)
      } else {
        if (!is.numeric(result) || !all(c("distance", "duration") %in% names(result))) {
          stop("The routing service returned an invalid result.", call. = FALSE)
        }
        results[[i]] <- c(
          distance = as.numeric(result[["distance"]]),
          duration = as.numeric(result[["duration"]])
        )
      }
      if (delay > 0 && i < length(unique_rows)) Sys.sleep(delay)
    }

    result_matrix <- do.call(rbind, results)
    matched <- match(keys, unique_keys)
    distance[valid_rows] <- result_matrix[matched, "distance"]
    duration[valid_rows] <- result_matrix[matched, "duration"]
    if (length(failures) && on_error == "warn") {
      warning(length(failures), " route request(s) failed; results were set to NA. First failure: ",
              failures[1L], call. = FALSE)
    }
  }

  output <- dataset
  output[[distance_column]] <- distance
  output[[time_column]] <- duration
  output
}

moose_osrm_route <- function(start_lon, start_lat, end_lon, end_lat, profile,
                             server, username = NULL, password = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required by `Moose_travel()`. Install it with install.packages(\"jsonlite\").", call. = FALSE)
  }
  coordinates <- paste0(
    format(start_lon, scientific = FALSE, trim = TRUE, digits = 15), ",",
    format(start_lat, scientific = FALSE, trim = TRUE, digits = 15), ";",
    format(end_lon, scientific = FALSE, trim = TRUE, digits = 15), ",",
    format(end_lat, scientific = FALSE, trim = TRUE, digits = 15)
  )
  request_url <- paste0(
    server, "/route/v1/", utils::URLencode(profile, reserved = TRUE), "/",
    coordinates, "?overview=false&steps=false&alternatives=false"
  )
  if (is.null(username)) {
    response <- jsonlite::fromJSON(request_url, simplifyVector = TRUE)
  } else {
    token <- jsonlite::base64_enc(charToRaw(enc2utf8(paste0(username, ":", password))))
    connection <- base::url(
      request_url,
      open = "rb",
      headers = c(Authorization = paste("Basic", token))
    )
    on.exit(close(connection), add = TRUE)
    response <- jsonlite::fromJSON(connection, simplifyVector = TRUE)
  }
  if (is.null(response$code) || !identical(response$code, "Ok") ||
      is.null(response$routes) || !nrow(response$routes)) {
    code <- if (is.null(response$code)) "unknown response" else response$code
    stop("OSRM did not return a route (", code, ").", call. = FALSE)
  }
  c(
    distance = as.numeric(response$routes$distance[1L]),
    duration = as.numeric(response$routes$duration[1L])
  )
}
