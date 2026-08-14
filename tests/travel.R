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
