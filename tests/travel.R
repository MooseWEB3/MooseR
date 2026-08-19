library(MooseR)

trips <- data.frame(
  start_lat = c(53.5, 53.5, NA, 95, 51.0),
  start_lon = c(-113.5, -113.5, -113.5, -113.5, -114.0),
  end_lat = c(53.6, 53.6, 53.6, 53.6, 51.1),
  end_lon = c(-113.6, -113.6, -113.6, -113.6, -114.1)
)

counter <- new.env(parent = emptyenv())
counter$n <- 0L
fake_route <- function(start_lon, start_lat, end_lon, end_lat, profile, server,
                       username, password) {
  counter$n <- counter$n + 1L
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
  profile = "driving",
  server = "https://example.test",
  username = NULL,
  password = NULL,
  distance_column = "distance_m",
  time_column = "time_s",
  overwrite = FALSE,
  on_error = "stop",
  timeout = 10,
  delay = 0,
  progress = FALSE,
  route_fun = fake_route
)

stopifnot(
  counter$n == 2L,
  identical(names(result), c(names(trips), "distance_m", "time_s")),
  isTRUE(all.equal(result$distance_m[c(1, 2, 5)], c(10000, 10000, 10000))),
  isTRUE(all.equal(result$time_s[c(1, 2, 5)], c(1000, 1000, 1000))),
  all(is.na(result$distance_m[c(3, 4)])),
  all(is.na(result$time_s[c(3, 4)]))
)

failed <- MooseR:::moose_travel_impl(
  trips[1, ], "start_lat", "start_lon", "end_lat", "end_lon",
  "driving", "https://example.test", NULL, NULL, "distance_m", "time_s",
  FALSE, "na", 10, 0, FALSE,
  route_fun = function(...) stop("test failure")
)
stopifnot(is.na(failed$distance_m), is.na(failed$time_s))

stopifnot(
  inherits(tryCatch(Moose_travel(1:3, "a", "b", "c", "d"), error = identity), "error"),
  inherits(tryCatch(Moose_travel(trips, "start_lat", "start_lon", "end_lat", "end_lon", username = "user"), error = identity), "error"),
  inherits(tryCatch(Moose_travel(trips, "missing", "start_lon", "end_lat", "end_lon"), error = identity), "error"),
  inherits(tryCatch(Moose_travel(trips, "start_lat", "start_lon", "end_lat", "end_lon", distance_column = "start_lat"), error = identity), "error")
)

one_trip <- trips[1L, , drop = FALSE]
captured_requests <- list()
success_mock <- function(req) {
  captured_requests[[length(captured_requests) + 1L]] <<- req
  httr2::response(
    status_code = 200,
    url = req$url,
    method = "GET",
    headers = list(`content-type` = "application/json"),
    body = charToRaw('{"code":"Ok","routes":[{"distance":1234,"duration":321}]}')
  )
}

authenticated_result <- httr2::with_mocked_responses(
  success_mock,
  Moose_travel(
    one_trip,
    "start_lat", "start_lon", "end_lat", "end_lon",
    server = "https://osrm.example.test",
    username = "proxy-user",
    password = " proxy-password ",
    delay = 0,
    progress = FALSE
  )
)
authenticated_request <- captured_requests[[1L]]
authenticated_print <- paste(capture.output(print(authenticated_request)), collapse = "\n")
authenticated_str <- paste(capture.output(str(authenticated_request)), collapse = "\n")
basic_token <- jsonlite::base64_enc(charToRaw("proxy-user: proxy-password "))
authorization_value <- get(
  "wref_value",
  envir = asNamespace("httr2"),
  inherits = TRUE
)(authenticated_request$headers[["Authorization"]])
stopifnot(
  identical(authenticated_request$options$followlocation, 0L),
  "Authorization" %in% names(authenticated_request$headers),
  identical(authorization_value, paste("Basic", basic_token)),
  isTRUE(all.equal(authenticated_result$travel_distance_meters, 1234)),
  isTRUE(all.equal(authenticated_result$travel_time_seconds, 321)),
  !grepl("proxy-user", authenticated_print, fixed = TRUE),
  !grepl("proxy-password", authenticated_print, fixed = TRUE),
  !grepl(basic_token, authenticated_print, fixed = TRUE),
  !grepl("proxy-user", authenticated_str, fixed = TRUE),
  !grepl("proxy-password", authenticated_str, fixed = TRUE),
  !grepl(basic_token, authenticated_str, fixed = TRUE)
)

http_without_credentials <- MooseR:::moose_osrm_request(
  start_lon = -113.5,
  start_lat = 53.5,
  end_lon = -113.6,
  end_lat = 53.6,
  profile = "driving",
  server = "http://127.0.0.1:5000",
  request_timeout = 10
)
stopifnot(
  identical(http_without_credentials$options$followlocation, 0L),
  !"Authorization" %in% names(http_without_credentials$headers)
)

http_with_credentials <- tryCatch(
  Moose_travel(
    one_trip,
    "start_lat", "start_lon", "end_lat", "end_lon",
    server = "http://osrm.example.test",
    username = "proxy-user",
    password = "proxy-password",
    delay = 0,
    progress = FALSE
  ),
  error = identity
)
userinfo_server <- tryCatch(
  Moose_travel(
    one_trip,
    "start_lat", "start_lon", "end_lat", "end_lon",
    server = "https://embedded-user:embedded-password@osrm.example.test",
    delay = 0,
    progress = FALSE
  ),
  error = identity
)
stopifnot(
  inherits(http_with_credentials, "error"),
  grepl("HTTPS is required", conditionMessage(http_with_credentials), fixed = TRUE),
  inherits(userinfo_server, "error"),
  grepl("must not contain URL userinfo", conditionMessage(userinfo_server), fixed = TRUE)
)
