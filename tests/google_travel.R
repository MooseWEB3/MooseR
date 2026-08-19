library(MooseR)

trips <- data.frame(
  start_lat = c(53.5, 53.5, NA, 95, 51.0),
  start_lon = c(-113.5, -113.5, -113.5, -113.5, -114.0),
  end_lat = c(53.6, 53.6, 53.6, 53.6, 51.1),
  end_lon = c(-113.6, -113.6, -113.6, -113.6, -114.1)
)

counter <- new.env(parent = emptyenv())
counter$n <- 0L
counter$preferences <- character()
fake_google_route <- function(start_lon, start_lat, end_lon, end_lat, profile,
                              server, username, password, backend, api_key,
                              routing_preference, request_timeout) {
  counter$n <- counter$n + 1L
  counter$preferences <- c(counter$preferences, routing_preference)
  stopifnot(
    identical(profile, "DRIVE"),
    identical(backend, "google"),
    identical(api_key, "test-key"),
    identical(request_timeout, 10)
  )
  c(
    distance = abs(end_lon - start_lon) * 100000,
    duration = abs(end_lat - start_lat) * 10000
  )
}

result <- MooseR:::moose_travel_impl(
  dataset = trips,
  start_latitude = "start_lat",
  start_longitude = "start_lon",
  end_latitude = "end_lat",
  end_longitude = "end_lon",
  profile = "DRIVE",
  server = "https://routes.googleapis.test",
  username = NULL,
  password = NULL,
  distance_column = "distance_m",
  time_column = "time_s",
  overwrite = FALSE,
  on_error = "stop",
  timeout = 10,
  delay = 0,
  progress = FALSE,
  route_fun = fake_google_route,
  route_args = list(
    backend = "google",
    api_key = "test-key",
    routing_preference = "TRAFFIC_UNAWARE",
    request_timeout = 10
  )
)

stopifnot(!"server" %in% names(formals(Moose_google_travel)))

mock_requests <- list()
success_mock <- function(req) {
  mock_requests[[length(mock_requests) + 1L]] <<- req
  httr2::response(
    status_code = 200,
    url = req$url,
    method = "POST",
    headers = list(`content-type` = "application/json"),
    body = charToRaw('{"routes":[{"distanceMeters":346,"duration":"74s"}]}')
  )
}

one_trip <- trips[1L, , drop = FALSE]
direct_result <- httr2::with_mocked_responses(
  success_mock,
  Moose_google_travel(
    one_trip,
    "start_lat", "start_lon", "end_lat", "end_lon",
    api_key = "unit-test-google-key",
    delay = 0,
    progress = FALSE
  )
)
direct_request <- mock_requests[[1L]]
direct_headers <- names(direct_request$headers)
direct_printed <- paste(capture.output(print(direct_request)), collapse = "\n")
direct_structured <- paste(capture.output(str(direct_request)), collapse = "\n")
stopifnot(
  identical(
    direct_request$url,
    "https://routes.googleapis.com/directions/v2:computeRoutes"
  ),
  "X-Goog-Api-Key" %in% direct_headers,
  "X-Goog-FieldMask" %in% direct_headers,
  !"Authorization" %in% direct_headers,
  identical(direct_request$body$data$travelMode, "DRIVE"),
  identical(direct_request$body$data$routingPreference, "TRAFFIC_UNAWARE"),
  identical(direct_request$body$data$units, "METRIC"),
  identical(direct_request$options$followlocation, 0L),
  isTRUE(all.equal(direct_result$travel_distance_meters, 346)),
  isTRUE(all.equal(direct_result$travel_time_seconds, 74)),
  !grepl("unit-test-google-key", direct_printed, fixed = TRUE),
  !grepl("unit-test-google-key", direct_structured, fixed = TRUE)
)

mock_requests <- list()
proxy_result <- httr2::with_mocked_responses(
  success_mock,
  Moose_google_travel(
    one_trip,
    "start_lat", "start_lon", "end_lat", "end_lon",
    api_key = "must-not-be-sent",
    backend = "mooser_proxy",
    username = "proxy-user",
    password = "proxy-password",
    delay = 0,
    progress = FALSE
  )
)
proxy_request <- mock_requests[[1L]]
proxy_headers <- names(proxy_request$headers)
proxy_printed <- paste(capture.output(print(proxy_request)), collapse = "\n")
proxy_structured <- paste(capture.output(str(proxy_request)), collapse = "\n")
proxy_basic_token <- jsonlite::base64_enc(
  charToRaw("proxy-user:proxy-password")
)
stopifnot(
  identical(
    proxy_request$url,
    "https://google-routes.mooseweb3.com/directions/v2:computeRoutes"
  ),
  "Authorization" %in% proxy_headers,
  "X-Goog-FieldMask" %in% proxy_headers,
  !"X-Goog-Api-Key" %in% proxy_headers,
  identical(proxy_request$options$followlocation, 0L),
  isTRUE(all.equal(proxy_result$travel_distance_meters, 346)),
  isTRUE(all.equal(proxy_result$travel_time_seconds, 74)),
  !grepl("proxy-user", proxy_printed, fixed = TRUE),
  !grepl("proxy-password", proxy_printed, fixed = TRUE),
  !grepl(proxy_basic_token, proxy_printed, fixed = TRUE),
  !grepl(proxy_basic_token, proxy_structured, fixed = TRUE),
  !grepl(
    "must-not-be-sent",
    proxy_structured,
    fixed = TRUE
  )
)

old_proxy_user <- Sys.getenv("MOOSER_GOOGLE_USER", unset = NA_character_)
old_proxy_password <- Sys.getenv("MOOSER_GOOGLE_PASSWORD", unset = NA_character_)
Sys.setenv(
  MOOSER_GOOGLE_USER = "environment-user",
  MOOSER_GOOGLE_PASSWORD = "environment-password"
)
mock_requests <- list()
environment_result <- httr2::with_mocked_responses(
  success_mock,
  Moose_google_travel(
    one_trip,
    "start_lat", "start_lon", "end_lat", "end_lon",
    api_key = "must-not-be-sent-from-environment-mode",
    backend = "mooser_proxy",
    delay = 0,
    progress = FALSE
  )
)
environment_request <- mock_requests[[1L]]
stopifnot(
  "Authorization" %in% names(environment_request$headers),
  !"X-Goog-Api-Key" %in% names(environment_request$headers),
  isTRUE(all.equal(environment_result$travel_distance_meters, 346))
)
if (is.na(old_proxy_user)) {
  Sys.unsetenv("MOOSER_GOOGLE_USER")
} else {
  Sys.setenv(MOOSER_GOOGLE_USER = old_proxy_user)
}
if (is.na(old_proxy_password)) {
  Sys.unsetenv("MOOSER_GOOGLE_PASSWORD")
} else {
  Sys.setenv(MOOSER_GOOGLE_PASSWORD = old_proxy_password)
}

mock_requests <- list()
suppressWarnings(httr2::with_mocked_responses(
  success_mock,
  Moose_google_travel(
    one_trip,
    "start_lat", "start_lon", "end_lat", "end_lon",
    api_key = "unit-test-google-key",
    travel_mode = "WALK",
    routing_preference = "IGNORED_VALUE",
    delay = 0,
    progress = FALSE
  )
))
walk_request <- mock_requests[[1L]]
stopifnot(
  identical(walk_request$body$data$travelMode, "WALK"),
  is.null(walk_request$body$data$routingPreference)
)

server_error_mock <- function(req) {
  httr2::response(
    status_code = 500,
    url = req$url,
    method = "POST",
    headers = list(`content-type` = "application/json"),
    body = charToRaw('{"error":{"message":"test failure"}}')
  )
}
failed_result <- httr2::with_mocked_responses(
  server_error_mock,
  Moose_google_travel(
    one_trip,
    "start_lat", "start_lon", "end_lat", "end_lon",
    api_key = "unit-test-google-key",
    on_error = "na",
    delay = 0,
    progress = FALSE
  )
)
stopifnot(
  is.na(failed_result$travel_distance_meters),
  is.na(failed_result$travel_time_seconds)
)

unsafe_request <- tryCatch(
  MooseR:::moose_google_request(
    start_lon = -113.5,
    start_lat = 53.5,
    end_lon = -113.6,
    end_lat = 53.6,
    profile = "DRIVE",
    server = "https://example.invalid",
    backend = "google",
    api_key = "unit-test-google-key",
    routing_preference = "TRAFFIC_UNAWARE",
    request_timeout = 10
  ),
  error = identity
)
stopifnot(inherits(unsafe_request, "error"))

stopifnot(
  inherits(
    tryCatch(
      Moose_google_travel(
        one_trip,
        "start_lat", "start_lon", "end_lat", "end_lon",
        api_key = "unit-test-google-key",
        username = "not-used"
      ),
      error = identity
    ),
    "error"
  ),
  inherits(
    tryCatch(
      Moose_google_travel(
        one_trip,
        "start_lat", "start_lon", "end_lat", "end_lon",
        api_key = "",
        backend = "mooser_proxy",
        username = "proxy-user",
        password = ""
      ),
      error = identity
    ),
    "error"
  ),
  inherits(
    tryCatch(
      Moose_google_travel(
        one_trip,
        "start_lat", "start_lon", "end_lat", "end_lon",
        api_key = "",
        backend = "mooser_proxy",
        username = "proxy-user"
      ),
      error = identity
    ),
    "error"
  )
)

stopifnot(
  counter$n == 2L,
  identical(counter$preferences, rep("TRAFFIC_UNAWARE", 2L)),
  isTRUE(all.equal(result$distance_m[c(1, 2, 5)], c(10000, 10000, 10000))),
  isTRUE(all.equal(result$time_s[c(1, 2, 5)], c(1000, 1000, 1000))),
  all(is.na(result$distance_m[c(3, 4)])),
  all(is.na(result$time_s[c(3, 4)]))
)

stopifnot(
  inherits(
    tryCatch(
      Moose_google_travel(trips, "start_lat", "start_lon", "end_lat", "end_lon", api_key = ""),
      error = identity
    ),
    "error"
  ),
  inherits(
    tryCatch(
      Moose_google_travel(trips, "start_lat", "start_lon", "end_lat", "end_lon",
                          api_key = "test-key", travel_mode = "PLANE"),
      error = identity
    ),
    "error"
  )
)
