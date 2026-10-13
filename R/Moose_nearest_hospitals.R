#' Add the three nearest hospitals by road distance
#'
#' For each input location, queries road routes to every eligible hospital and
#' appends the names, travel times and travel distances of the nearest three.
#' By default, candidates are all facilities in [alberta_hospitals].
#'
#' Hospitals are ranked by increasing OSRM route distance, then travel time,
#' then their row order in `hospitals`. OSRM returns fastest routes: their
#' distances are not guaranteed to be the shortest possible road paths. Times
#' are estimates without live traffic. Separately named facilities on the same
#' campus remain separate candidates. Coordinates refer to facility locations,
#' not verified emergency-department entrances.
#'
#' Uses the OSRM Table service with both distance and duration annotations.
#' Every eligible hospital is considered; there is no straight-line prefilter.
#' Repeated input coordinates are queried once per call. A failed request leaves
#' all nine results for that origin as `NA`, because an incomplete comparison
#' cannot establish the nearest hospitals. Unreachable hospitals are excluded;
#' if fewer than three are reachable, the remaining ranks are `NA`.
#'
#' The default public demonstration server is for light, interactive use. For
#' larger workloads, supply an OSRM server you operate or are authorized to use.
#' Coordinates are sent to that server and may appear in its request logs.
#' Redirects are not followed. No API key is needed for the default server.
#'
#' @param dataset A data frame or tibble. Original rows and columns are retained.
#' @param longitude,latitude Names of columns containing origin coordinates in
#'   decimal degrees. Numeric strings and factors are accepted. Missing,
#'   non-numeric, non-finite or out-of-range coordinates return nine `NA` values
#'   without making a request.
#' @param hospitals Optional candidate data frame with unique, non-empty
#'   `hospital_name` values and valid `longitude` and `latitude` columns. `NULL`
#'   uses the bundled [alberta_hospitals]. At least three eligible rows are
#'   required. Other columns are ignored except `emergency_24h` when filtering.
#' @param emergency_only Logical. If `TRUE`, restrict candidates to rows with
#'   `emergency_24h == 1`. This flag describes regular 24-hour emergency service,
#'   not temporary closures or current availability.
#' @param server HTTP(S) base URL of an OSRM server supporting the Table service
#'   with distance and duration annotations. Defaults to the public demo server.
#' @param profile OSRM profile supported by the server, normally `"driving"`.
#' @param username,password Optional HTTP Basic Authentication credentials.
#'   Supply both or neither. HTTPS is required with credentials; URL userinfo is
#'   rejected. Credentials are redacted from printed request objects.
#' @param batch_size Maximum destinations per request, an integer from 1 to 99.
#'   One origin is added, keeping each request within 100 coordinates. Reduce
#'   this value if your server has a lower limit.
#' @param overwrite Logical. Replace existing output columns if `TRUE`.
#' @param on_error One of `"warn"`, `"stop"`, or `"na"`. Controls request failures
#'   and fewer than three reachable hospitals. `"warn"` gives a summary warning;
#'   `"na"` returns the available results silently. Invalid input coordinates
#'   always return `NA`, regardless of this setting.
#' @param timeout Positive seconds allowed for each request.
#' @param delay Non-negative seconds to pause between requests.
#' @param progress Logical. Print progress for unique input locations.
#'
#' @return The input data frame with exactly nine output columns: for each
#'   `i` in 1, 2, 3, `nearest_hospital_i` (character),
#'   `nearest_hospital_i_travel_time_seconds` (numeric) and
#'   `nearest_hospital_i_travel_distance_meters` (numeric).
#' @seealso [alberta_hospitals], [Moose_travel()]
#' @references \url{https://project-osrm.org/docs/v5.24.0/api/#table-service}
#' @examples
#' \dontrun{
#' locations <- data.frame(longitude = c(-113.4938, -114.0719),
#'                         latitude = c(53.5461, 51.0447))
#' result <- Moose_nearest_hospitals(locations)
#' emergency_result <- Moose_nearest_hospitals(locations, emergency_only = TRUE)
#'
#' # Custom column names and a local routing server:
#' locations <- data.frame(lon = -113.4938, lat = 53.5461)
#' result <- Moose_nearest_hospitals(locations, longitude = "lon", latitude = "lat",
#'                                   server = "http://127.0.0.1:5001")
#' }
#' @export
Moose_nearest_hospitals <- function(dataset,
                                    longitude = "longitude",
                                    latitude = "latitude",
                                    hospitals = NULL,
                                    emergency_only = FALSE,
                                    server = "https://router.project-osrm.org",
                                    profile = "driving",
                                    username = NULL,
                                    password = NULL,
                                    batch_size = 99L,
                                    overwrite = FALSE,
                                    on_error = c("warn", "stop", "na"),
                                    timeout = 60,
                                    delay = 0.1,
                                    progress = interactive()) {
  validate_text <- function(x, argument) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      stop("`", argument, "` must be one non-empty character value.", call. = FALSE)
    }
    x
  }
  validate_flag <- function(x, argument) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`", argument, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  validate_number <- function(x, argument, minimum, inclusive = TRUE) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
        (if (inclusive) x < minimum else x <= minimum)) {
      stop("`", argument, "` must be a finite number ",
           if (inclusive) ">= " else "> ", minimum, ".", call. = FALSE)
    }
  }
  as_coordinate <- function(x) {
    if (!is.atomic(x) || !is.null(dim(x))) {
      stop("Coordinate columns must be atomic vectors.", call. = FALSE)
    }
    suppressWarnings(as.numeric(as.character(x)))
  }
  valid_coordinates <- function(lon, lat) {
    is.finite(lon) & abs(lon) <= 180 & is.finite(lat) & abs(lat) <= 90
  }

  if (!is.data.frame(dataset) || anyDuplicated(names(dataset))) {
    stop("`dataset` must be a data frame or tibble with unique column names.", call. = FALSE)
  }
  longitude <- validate_text(longitude, "longitude")
  latitude <- validate_text(latitude, "latitude")
  missing_columns <- setdiff(c(longitude, latitude), names(dataset))
  if (length(missing_columns)) {
    stop("Unknown coordinate columns: ", paste(missing_columns, collapse = ", "), ".", call. = FALSE)
  }
  validate_flag(emergency_only, "emergency_only")
  validate_flag(overwrite, "overwrite")
  validate_flag(progress, "progress")
  on_error <- match.arg(on_error)
  validate_number(timeout, "timeout", 0, inclusive = FALSE)
  validate_number(delay, "delay", 0)
  validate_number(batch_size, "batch_size", 1)
  if (batch_size > 99 || batch_size != floor(batch_size)) {
    stop("`batch_size` must be an integer from 1 to 99.", call. = FALSE)
  }
  profile <- trimws(validate_text(profile, "profile"))
  server <- trimws(validate_text(server, "server"))
  credentials <- c(!is.null(username), !is.null(password))
  if (any(credentials) && !all(credentials)) {
    stop("`username` and `password` must be supplied together.", call. = FALSE)
  }
  if (all(credentials)) {
    username <- trimws(validate_text(username, "username"))
    password <- validate_text(password, "password")
  }
  server <- moose_validate_routing_server(server, authenticated = all(credentials))

  output_names <- unlist(lapply(1:3, function(i) {
    paste0("nearest_hospital_", i, c("", "_travel_time_seconds", "_travel_distance_meters"))
  }), use.names = FALSE)
  conflicts <- intersect(output_names, names(dataset))
  if (length(conflicts) && !overwrite) {
    stop("Output columns already exist: ", paste(conflicts, collapse = ", "),
         ". Use `overwrite = TRUE` to replace them.", call. = FALSE)
  }

  if (is.null(hospitals)) {
    bundled <- new.env(parent = emptyenv())
    utils::data("alberta_hospitals", package = "MooseR", envir = bundled)
    hospitals <- bundled$alberta_hospitals
  }
  required <- c("hospital_name", "longitude", "latitude",
                if (emergency_only) "emergency_24h")
  if (!is.data.frame(hospitals) || anyDuplicated(names(hospitals)) ||
      !all(required %in% names(hospitals))) {
    stop("`hospitals` must be a data frame with columns: ",
         paste(required, collapse = ", "), ".", call. = FALSE)
  }
  if (emergency_only) {
    flags <- as_coordinate(hospitals$emergency_24h)
    if (anyNA(flags) || !all(flags %in% c(0, 1))) {
      stop("`hospitals$emergency_24h` must contain only 0 and 1.", call. = FALSE)
    }
    hospitals <- hospitals[flags == 1, , drop = FALSE]
  }
  if (nrow(hospitals) < 3L) {
    stop("At least three eligible hospitals are required.", call. = FALSE)
  }
  if (!is.character(hospitals$hospital_name) && !is.factor(hospitals$hospital_name)) {
    stop("`hospitals$hospital_name` must contain hospital names.", call. = FALSE)
  }
  hospital_names <- as.character(hospitals$hospital_name)
  if (anyNA(hospital_names) || any(!nzchar(trimws(hospital_names))) ||
      anyDuplicated(hospital_names)) {
    stop("Eligible hospital names must be non-empty, non-missing and unique.", call. = FALSE)
  }
  hospital_lon <- as_coordinate(hospitals$longitude)
  hospital_lat <- as_coordinate(hospitals$latitude)
  if (!all(valid_coordinates(hospital_lon, hospital_lat))) {
    stop("Eligible hospital coordinates must be valid decimal degrees.", call. = FALSE)
  }

  origin_lon <- as_coordinate(dataset[[longitude]])
  origin_lat <- as_coordinate(dataset[[latitude]])
  valid_rows <- which(valid_coordinates(origin_lon, origin_lat))
  result <- dataset
  for (j in seq_along(output_names)) {
    result[[output_names[j]]] <- if (j %% 3L == 1L) {
      rep(NA_character_, nrow(dataset))
    } else {
      rep(NA_real_, nrow(dataset))
    }
  }
  if (!length(valid_rows)) return(result)

  keys <- sprintf("%.17g|%.17g", origin_lon[valid_rows], origin_lat[valid_rows])
  unique_positions <- which(!duplicated(keys))
  unique_rows <- valid_rows[unique_positions]
  row_groups <- split(valid_rows, match(keys, keys[unique_positions]))
  batches <- split(seq_len(nrow(hospitals)),
                   ceiling(seq_len(nrow(hospitals)) / batch_size))
  request_count <- 0L
  failures <- character()
  for (i in seq_along(unique_rows)) {
    origin <- unique_rows[i]
    if (progress) message("Finding hospitals for location ", i, " of ", length(unique_rows), "...")
    routes <- tryCatch({
      distances <- durations <- rep(NA_real_, nrow(hospitals))
      for (batch in batches) {
        if (request_count > 0L && delay > 0) Sys.sleep(delay)
        request_count <- request_count + 1L
        table <- moose_osrm_table(
          origin_lon[origin], origin_lat[origin],
          hospital_lon[batch], hospital_lat[batch],
          profile, server, username, password, timeout
        )
        distances[batch] <- table$distance
        durations[batch] <- table$duration
      }
      list(distance = distances, duration = durations)
    }, error = identity)

    issue <- NULL
    if (inherits(routes, "error")) {
      issue <- conditionMessage(routes)
    } else {
      reachable <- which(!is.na(routes$distance))
      ranked <- reachable[order(routes$distance[reachable],
                                routes$duration[reachable], reachable)]
      selected <- utils::head(ranked, 3L)
      rows <- row_groups[[as.character(i)]]
      for (rank in seq_along(selected)) {
        hospital <- selected[rank]
        offset <- (rank - 1L) * 3L
        result[[output_names[offset + 1L]]][rows] <- hospital_names[hospital]
        result[[output_names[offset + 2L]]][rows] <- routes$duration[hospital]
        result[[output_names[offset + 3L]]][rows] <- routes$distance[hospital]
      }
      if (length(reachable) < 3L) {
        issue <- paste0("Only ", length(reachable), " hospitals are reachable; unfilled ranks are NA.")
      }
    }
    if (!is.null(issue)) {
      issue <- paste0("Input row ", origin, ": ", issue)
      if (on_error == "stop") stop(issue, call. = FALSE)
      failures <- c(failures, issue)
    }
  }
  if (length(failures) && on_error == "warn") {
    warning(length(failures), " unique location(s) had incomplete hospital results. First issue: ",
            failures[1L], call. = FALSE)
  }
  result
}

# Query a single source against a batch of destinations. Preserve JSON nulls
# until matrix dimensions and scalar values have been checked.
moose_osrm_table <- function(start_lon, start_lat, end_lon, end_lat, profile,
                             server, username, password, request_timeout) {
  coordinates <- paste(
    paste0(format(c(start_lon, end_lon), scientific = FALSE, trim = TRUE, digits = 17),
           ",", format(c(start_lat, end_lat), scientific = FALSE, trim = TRUE, digits = 17)),
    collapse = ";"
  )
  request_url <- paste0(
    server, "/table/v1/", utils::URLencode(profile, reserved = TRUE), "/", coordinates,
    "?sources=0&destinations=", paste(seq_along(end_lon), collapse = ";"),
    "&annotations=duration,distance"
  )
  request <- httr2::request(request_url) |>
    httr2::req_timeout(request_timeout) |>
    httr2::req_options(followlocation = 0L)
  if (!is.null(username)) {
    token <- jsonlite::base64_enc(charToRaw(enc2utf8(paste0(username, ":", password))))
    request <- httr2::req_headers_redacted(request, Authorization = paste("Basic", token))
  }
  response <- httr2::req_perform(request)
  payload <- httr2::resp_body_json(response, check_type = FALSE, simplifyVector = FALSE)
  if (!is.list(payload) || !identical(payload$code, "Ok")) {
    stop("OSRM did not return a successful route table.", call. = FALSE)
  }
  read_row <- function(matrix, field) {
    if (!is.list(matrix) || length(matrix) != 1L || !is.list(matrix[[1L]]) ||
        length(matrix[[1L]]) != length(end_lon)) {
      stop("OSRM returned an invalid `", field, "` matrix.", call. = FALSE)
    }
    vapply(matrix[[1L]], function(value) {
      if (is.null(value)) return(NA_real_)
      if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
        stop("OSRM returned an invalid `", field, "` value.", call. = FALSE)
      }
      as.numeric(value)
    }, numeric(1))
  }
  distances <- read_row(payload$distances, "distances")
  durations <- read_row(payload$durations, "durations")
  if (any(is.na(distances) != is.na(durations))) {
    stop("OSRM returned inconsistent distance and duration availability.", call. = FALSE)
  }
  list(distance = distances, duration = durations)
}
