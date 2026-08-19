.mooser_google_server <- "https://routes.googleapis.com"
.mooser_google_proxy_server <- "https://google-routes.mooseweb3.com"

#' Add Google road travel distance and time to a data set
#'
#' `Moose_google_travel()` is currently in development and has not been
#' formally released. Its interface and returned results may change.
#'
#' Looks up one Google Routes API route for each unique valid
#' origin-destination coordinate pair. By default, current traffic conditions
#' are not used, which keeps requests in the Compute Routes Essentials tier
#' when no other requested feature raises the billing tier.
#'
#' Google Maps Platform billing must be enabled and the Routes API must be
#' enabled for the project associated with `api_key`. Keep API keys outside
#' source code and restrict them to the Routes API and appropriate server IPs.
#'
#' `backend = "google"` sends requests only to Google's fixed Routes API
#' endpoint and uses `api_key`. `backend = "mooser_proxy"` sends requests only
#' to the fixed MooseR HTTPS proxy and uses HTTP Basic Authentication; the
#' Google API key is neither required nor sent by the client in proxy mode. The
#' proxy must be deployed and configured separately before that mode can work.
#'
#' Requests transmit the supplied coordinates to Google, either directly or
#' through the MooseR proxy. Google Maps Platform content is subject to Google's
#' attribution, caching, and storage rules. Review the
#' [Google Maps Platform Terms](https://cloud.google.com/maps-platform/terms)
#' and the
#' [Routes API policies](https://developers.google.com/maps/documentation/routes/policies)
#' before storing, displaying, or redistributing results.
#'
#' @param dataset A data frame or tibble.
#' @param start_latitude,start_longitude Column names containing origin
#'   latitude and longitude.
#' @param end_latitude,end_longitude Column names containing destination
#'   latitude and longitude.
#' @param api_key A Google Maps Platform API key. Defaults to the
#'   `GOOGLE_MAPS_API_KEY` environment variable. Used only by
#'   `backend = "google"`; it is never sent in proxy mode.
#' @param backend Routing backend. `"google"` calls Google's fixed official
#'   endpoint directly. `"mooser_proxy"` calls the fixed MooseR HTTPS proxy
#'   using `username` and `password`.
#' @param username,password HTTP Basic Authentication credentials for
#'   `backend = "mooser_proxy"`. Supply both or neither. When omitted in proxy
#'   mode, the function reads `MOOSER_GOOGLE_USER` and
#'   `MOOSER_GOOGLE_PASSWORD`. They are never sent to Google directly. Do not
#'   write credentials into package code or scripts committed to Git.
#' @param travel_mode Google Routes travel mode. One of `"DRIVE"`, `"WALK"`,
#'   `"BICYCLE"`, `"TWO_WHEELER"`, or `"TRANSIT"`. Google currently bills
#'   `"TWO_WHEELER"` requests in the Compute Routes Enterprise tier.
#' @param routing_preference Driving-route preference. The default,
#'   `"TRAFFIC_UNAWARE"`, does not use current traffic. `"TRAFFIC_AWARE"` and
#'   `"TRAFFIC_AWARE_OPTIMAL"` use traffic-aware routing and currently trigger
#'   the Compute Routes Pro tier. This argument is omitted from requests for
#'   non-driving travel modes. Check Google's current pricing before large jobs.
#' @param distance_column Name of the output distance column, in metres.
#' @param time_column Name of the output travel-time column, in seconds.
#' @param overwrite Logical. Allow existing output columns to be replaced.
#' @param on_error One of `"warn"`, `"stop"`, or `"na"`. Controls request and
#'   no-route failures. Invalid or missing coordinates always return `NA`.
#' @param timeout Positive number of seconds allowed for each Google request.
#' @param delay Non-negative seconds to pause between unique route requests.
#' @param progress Logical. Print request progress.
#'
#' @return The input data frame with two numeric columns appended.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(GOOGLE_MAPS_API_KEY = "your_restricted_key")
#'
#' trips <- data.frame(
#'   start_lat = 53.5461,
#'   start_lon = -113.4938,
#'   end_lat = 53.5444,
#'   end_lon = -113.4909
#' )
#'
#' Moose_google_travel(
#'   trips,
#'   start_latitude = "start_lat",
#'   start_longitude = "start_lon",
#'   end_latitude = "end_lat",
#'   end_longitude = "end_lon"
#' )
#'
#' # A separately deployed MooseR proxy keeps the Google API key off clients.
#' Sys.setenv(
#'   MOOSER_GOOGLE_USER = "your_proxy_username",
#'   MOOSER_GOOGLE_PASSWORD = "your_proxy_password"
#' )
#' Moose_google_travel(
#'   trips,
#'   start_latitude = "start_lat",
#'   start_longitude = "start_lon",
#'   end_latitude = "end_lat",
#'   end_longitude = "end_lon",
#'   backend = "mooser_proxy"
#' )
#' }
#'
#' @export
Moose_google_travel <- function(dataset,
                                start_latitude,
                                start_longitude,
                                end_latitude,
                                end_longitude,
                                api_key = Sys.getenv("GOOGLE_MAPS_API_KEY"),
                                backend = c("google", "mooser_proxy"),
                                username = NULL,
                                password = NULL,
                                travel_mode = "DRIVE",
                                routing_preference = "TRAFFIC_UNAWARE",
                                distance_column = "travel_distance_meters",
                                time_column = "travel_time_seconds",
                                overwrite = FALSE,
                                on_error = c("warn", "stop", "na"),
                                timeout = 60,
                                delay = 0.1,
                                progress = interactive()) {
  routing_preference_supplied <- !missing(routing_preference)

  validate_choice <- function(value, argument, choices) {
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      stop("`", argument, "` must be one character value.", call. = FALSE)
    }
    value <- toupper(trimws(value))
    if (!value %in% choices) {
      stop("`", argument, "` must be one of: ",
           paste(choices, collapse = ", "), ".", call. = FALSE)
    }
    value
  }

  backend <- match.arg(backend)
  travel_mode <- validate_choice(
    travel_mode,
    "travel_mode",
    c("DRIVE", "WALK", "BICYCLE", "TWO_WHEELER", "TRANSIT")
  )
  if (travel_mode %in% c("DRIVE", "TWO_WHEELER")) {
    routing_preference <- validate_choice(
      routing_preference,
      "routing_preference",
      c("TRAFFIC_UNAWARE", "TRAFFIC_AWARE", "TRAFFIC_AWARE_OPTIMAL")
    )
  } else {
    if (routing_preference_supplied) {
      warning(
        "`routing_preference` is ignored unless `travel_mode` is `DRIVE` or `TWO_WHEELER`.",
        call. = FALSE
      )
    }
    routing_preference <- NULL
  }

  if (backend == "google") {
    if (!is.null(username) || !is.null(password)) {
      stop(
        "`username` and `password` are only used with `backend = \"mooser_proxy\"`.",
        call. = FALSE
      )
    }
    if (!is.character(api_key) || length(api_key) != 1L || is.na(api_key) ||
        !nzchar(trimws(api_key))) {
      stop(
        "A Google Routes API key is required. Set `GOOGLE_MAPS_API_KEY` or supply `api_key`.",
        call. = FALSE
      )
    }
    api_key <- trimws(api_key)
    server <- .mooser_google_server
  } else {
    credentials_supplied <- c(!is.null(username), !is.null(password))
    if (any(credentials_supplied) && !all(credentials_supplied)) {
      stop("`username` and `password` must be supplied together.", call. = FALSE)
    }
    if (!any(credentials_supplied)) {
      username <- Sys.getenv("MOOSER_GOOGLE_USER")
      password <- Sys.getenv("MOOSER_GOOGLE_PASSWORD")
    }
    credentials_valid <-
      is.character(username) && length(username) == 1L && !is.na(username) &&
      nzchar(trimws(username)) &&
      is.character(password) && length(password) == 1L && !is.na(password) &&
      nzchar(password)
    if (!credentials_valid) {
      stop(
        "Proxy credentials are required. Supply both `username` and `password`, or set `MOOSER_GOOGLE_USER` and `MOOSER_GOOGLE_PASSWORD`.",
        call. = FALSE
      )
    }
    username <- trimws(username)
    api_key <- NULL
    server <- .mooser_google_proxy_server
  }

  moose_travel_impl(
    dataset = dataset,
    start_latitude = start_latitude,
    start_longitude = start_longitude,
    end_latitude = end_latitude,
    end_longitude = end_longitude,
    profile = travel_mode,
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
    route_fun = moose_google_route,
    route_args = list(
      backend = backend,
      api_key = api_key,
      routing_preference = routing_preference,
      request_timeout = timeout
    )
  )
}

moose_google_route <- function(start_lon, start_lat, end_lon, end_lat,
                               profile, server, username = NULL,
                               password = NULL, backend, api_key,
                               routing_preference, request_timeout) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "Package `httr2` is required by `Moose_google_travel()`. Install it with install.packages(\"httr2\").",
      call. = FALSE
    )
  }

  request <- moose_google_request(
    start_lon = start_lon,
    start_lat = start_lat,
    end_lon = end_lon,
    end_lat = end_lat,
    profile = profile,
    server = server,
    username = username,
    password = password,
    backend = backend,
    api_key = api_key,
    routing_preference = routing_preference,
    request_timeout = request_timeout
  )

  response <- httr2::req_perform(request)
  payload <- httr2::resp_body_json(response, simplifyVector = TRUE)
  moose_google_parse_route(payload)
}

moose_google_request <- function(start_lon, start_lat, end_lon, end_lat,
                                 profile, server, username = NULL,
                                 password = NULL, backend, api_key,
                                 routing_preference, request_timeout) {
  expected_server <- switch(
    backend,
    google = .mooser_google_server,
    mooser_proxy = .mooser_google_proxy_server,
    stop("Unknown Google travel backend.", call. = FALSE)
  )
  if (!identical(server, expected_server)) {
    stop("Refusing to send routing credentials to an unapproved endpoint.", call. = FALSE)
  }

  waypoint <- function(latitude, longitude) {
    list(location = list(latLng = list(
      latitude = unname(latitude),
      longitude = unname(longitude)
    )))
  }
  body <- list(
    origin = waypoint(start_lat, start_lon),
    destination = waypoint(end_lat, end_lon),
    travelMode = profile,
    computeAlternativeRoutes = FALSE,
    languageCode = "en-US",
    units = "METRIC"
  )
  if (profile %in% c("DRIVE", "TWO_WHEELER")) {
    body$routingPreference <- routing_preference
  }

  request <- httr2::request(paste0(sub("/+$", "", server), "/directions/v2:computeRoutes")) |>
    httr2::req_headers(
      `X-Goog-FieldMask` = "routes.distanceMeters,routes.duration"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(request_timeout) |>
    httr2::req_options(followlocation = 0L)

  if (backend == "google") {
    request <- request |>
      httr2::req_headers_redacted(`X-Goog-Api-Key` = api_key)
  } else {
    request <- httr2::req_auth_basic(request, username, password)
  }
  request
}

moose_google_parse_route <- function(payload) {
  if (is.null(payload$routes) || !NROW(payload$routes)) {
    stop("Google Routes API did not return a route.", call. = FALSE)
  }
  duration_text <- as.character(payload$routes$duration[1L])
  duration <- suppressWarnings(as.numeric(sub("s$", "", duration_text)))
  distance <- suppressWarnings(as.numeric(payload$routes$distanceMeters[1L]))
  if (!is.finite(distance) || !is.finite(duration)) {
    stop("Google Routes API returned an invalid distance or duration.", call. = FALSE)
  }
  c(distance = distance, duration = duration)
}
