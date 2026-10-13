library(MooseR)

# Fixed route distances deliberately disagree with geographic proximity. The
# nearest candidates include the last batch and a distance tie decided by time.
hospitals <- data.frame(
  hospital_name = LETTERS[1:6],
  longitude = -113 - (1:6) / 10,
  latitude = rep(53.5, 6),
  emergency_24h = c(1L, 1L, 1L, 0L, 0L, 1L)
)
fixed_hospitals <- alberta_hospitals[match(
  c("Foothills Medical Centre", "University of Alberta Hospital"),
  alberta_hospitals$hospital_name
), names(hospitals)]
mock_hospitals <- rbind(hospitals, fixed_hospitals)
# Fixed destinations outside a custom candidate list must not enter its top 3,
# even when the fixed routes are shorter than every candidate route.
distances <- c(600, 100, 200, 400, 50, 100, 5, 1)
durations <- c(600, 30, 20, 40, 100, 10, 400, 500)
locations <- data.frame(
  id = 1:8,
  lon = factor(c("-113.5", "-114", "-113.5", NA, "bad", "181", "-113", "Inf")),
  lat = c(53.5, 51, 53.5, 53, 53, 53, -91, 53)
)
requests <- list()
seen_candidates <- integer()
response_json <- function(req, body, status = 200) {
  httr2::response(status_code = status, url = req$url, method = "GET",
                  headers = list(`content-type` = "application/json"),
                  body = charToRaw(body))
}
route_mock <- function(req) {
  requests[[length(requests) + 1L]] <<- req
  url <- httr2::url_parse(req$url)
  pairs <- strsplit(sub("^/table/v1/driving/", "", url$path), ";", fixed = TRUE)[[1L]]
  coords <- do.call(rbind, strsplit(pairs, ",", fixed = TRUE))
  stopifnot(url$query$sources == "0",
            url$query$destinations == paste(seq_len(nrow(coords) - 1L), collapse = ";"),
            url$query$annotations == "duration,distance")
  destinations <- match(as.numeric(coords[-1L, 1L]), mock_hospitals$longitude)
  stopifnot(!anyNA(destinations),
            all(as.numeric(coords[-1L, 2L]) == mock_hospitals$latitude[destinations]))
  seen_candidates <<- c(seen_candidates, destinations)
  # A second origin has different results, testing row alignment after reuse.
  offset <- if (as.numeric(coords[1L, 1L]) == -114) 1000 else 0
  response_json(req, jsonlite::toJSON(list(
    code = "Ok", distances = list(as.list(distances[destinations] + offset)),
    durations = list(as.list(durations[destinations] + offset))
  ), auto_unbox = TRUE))
}
run <- function(dataset = locations, ...) {
  Moose_nearest_hospitals(dataset, longitude = "lon", latitude = "lat",
                          hospitals = hospitals, server = "https://example.test",
                          batch_size = 2L, delay = 0, progress = FALSE, ...)
}
result <- httr2::with_mocked_responses(route_mock, run(on_error = "stop"))
columns <- c(
  "nearest_hospital_1", "nearest_hospital_1_travel_time_seconds",
  "nearest_hospital_1_travel_distance_meters",
  "nearest_hospital_2", "nearest_hospital_2_travel_time_seconds",
  "nearest_hospital_2_travel_distance_meters",
  "nearest_hospital_3", "nearest_hospital_3_travel_time_seconds",
  "nearest_hospital_3_travel_distance_meters"
)
nearest_columns <- columns
fixed_columns <- c(
  "foothills_medical_centre_travel_time_seconds",
  "foothills_medical_centre_travel_distance_meters",
  "university_of_alberta_hospital_travel_time_seconds",
  "university_of_alberta_hospital_travel_distance_meters"
)
columns <- c(columns, fixed_columns)
stopifnot(
  length(requests) == 8L, # Four batches per unique valid origin.
  identical(seen_candidates, rep(1:8, 2)),
  identical(names(result), c(names(locations), columns)),
  identical(result[names(locations)], locations),
  identical(unname(unlist(result[1, columns[c(1, 4, 7)]])), c("E", "F", "B")),
  identical(unname(unlist(result[1, columns[c(3, 6, 9)]])), c(50, 100, 100)),
  identical(unname(unlist(result[1, columns[c(2, 5, 8)]])), c(100, 10, 30)),
  identical(unname(unlist(result[2, columns[c(3, 6, 9)]])), c(1050, 1100, 1100)),
  identical(unname(unlist(result[1, fixed_columns])), c(400, 5, 500, 1)),
  identical(unname(unlist(result[2, fixed_columns])), c(1400, 1005, 1500, 1001)),
  identical(unname(unlist(result[1, columns])), unname(unlist(result[3, columns]))),
  all(is.na(result[4:8, columns])),
  all(vapply(result[columns[c(1, 4, 7)]], is.character, logical(1))),
  all(vapply(result[setdiff(columns, columns[c(1, 4, 7)])], is.double, logical(1)))
)

requests <- list()
seen_candidates <- integer()
filtered <- httr2::with_mocked_responses(
  route_mock, run(locations[1, ], emergency_only = TRUE, on_error = "stop")
)
stopifnot(length(requests) == 3L, identical(seen_candidates, c(1L, 2L, 3L, 6L, 7L, 8L)),
          identical(unname(unlist(filtered[columns[c(1, 4, 7)]])), c("F", "B", "C")),
          identical(unname(unlist(filtered[fixed_columns])), c(400, 5, 500, 1)))

# Zero rows and invalid origins preserve the schema and make no requests.
no_request <- function(req) stop("Unexpected request")
empty <- httr2::with_mocked_responses(no_request, run(locations[FALSE, ], on_error = "stop"))
invalid <- httr2::with_mocked_responses(no_request, run(locations[4:8, ], on_error = "stop"))
stopifnot(nrow(empty) == 0L, identical(names(empty), names(result)),
          all(is.na(invalid[columns])))

must_error <- function(expr, pattern) {
  error <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error), fixed = TRUE))
}
must_error(run(result), "Output columns already exist")
fixed_conflict <- locations
fixed_conflict[[fixed_columns[1]]] <- 999
must_error(run(fixed_conflict), fixed_columns[1])
replaced <- httr2::with_mocked_responses(route_mock, run(result, overwrite = TRUE))
stopifnot(identical(replaced, result))
must_error(Moose_nearest_hospitals(1:3), "data frame")
must_error(Moose_nearest_hospitals(locations), "Unknown coordinate columns")
must_error(Moose_nearest_hospitals(locations, longitude = c("id", "lon")), "longitude")
must_error(run(emergency_only = NA), "emergency_only")
must_error(run(username = "user"), "supplied together")
must_error(Moose_nearest_hospitals(data.frame(longitude = 0, latitude = 0),
  server = "http://example.test", username = "user", password = "secret"), "HTTPS")
must_error(Moose_nearest_hospitals(data.frame(longitude = 0, latitude = 0),
  server = "https://user:secret@example.test"), "userinfo")
must_error(Moose_nearest_hospitals(data.frame(longitude = 0, latitude = 0),
  batch_size = 1.5), "integer from 1 to 99")
must_error(Moose_nearest_hospitals(data.frame(longitude = 0, latitude = 0),
  hospitals = hospitals[1:2, ]), "At least three")
bad_hospitals <- hospitals
bad_hospitals$latitude[1] <- NA
must_error(Moose_nearest_hospitals(data.frame(longitude = 0, latitude = 0),
  hospitals = bad_hospitals), "hospital coordinates")
bad_hospitals <- hospitals
bad_hospitals$emergency_24h[1] <- NA
must_error(Moose_nearest_hospitals(data.frame(longitude = 0, latitude = 0),
  hospitals = bad_hospitals, emergency_only = TRUE), "only 0 and 1")

# A later batch failure must not produce an apparently complete ranking from
# the earlier successful batch. Verify all three error policies.
batch_calls <- 0L
partial_failure <- function(req) {
  batch_calls <<- batch_calls + 1L
  if (batch_calls %% 2L == 0L) return(response_json(req, '{"code":"TooBig"}', 400))
  route_mock(req)
}
failed <- httr2::with_mocked_responses(partial_failure, run(locations[1, ], on_error = "na"))
stopifnot(all(is.na(failed[columns])))
warning_text <- character()
warned <- withCallingHandlers(
  httr2::with_mocked_responses(partial_failure, run(locations[1, ], on_error = "warn")),
  warning = function(w) {
    warning_text <<- c(warning_text, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
stopifnot(length(warning_text) == 1L, grepl("Input row 1", warning_text),
          all(is.na(warned[columns])))
must_error(httr2::with_mocked_responses(partial_failure,
  run(locations[1, ], on_error = "stop")), "Input row 1")

# Null cells represent unreachable destinations, not a request failure. Ties
# (including co-located facilities) remain distinct and use candidate row order.
one_location <- data.frame(longitude = -113.5, latitude = 53.5)
fixed_run <- function(body, ...) {
  payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  for (field in c("distances", "durations")) {
    if (length(payload[[field]]) == 1L && length(payload[[field]][[1L]]) == 6L) {
      values <- if (field == "distances") c(5, 1) else c(400, 500)
      payload[[field]][[1L]] <- c(payload[[field]][[1L]], as.list(values))
    }
  }
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  httr2::with_mocked_responses(function(req) response_json(req, body),
    Moose_nearest_hospitals(one_location, hospitals = hospitals,
      server = "https://example.test", delay = 0, progress = FALSE, ...))
}
reachable <- fixed_run(
  '{"code":"Ok","distances":[[0,null,20,20,null,null]],"durations":[[0,null,10,10,null,null]]}',
  on_error = "stop"
)
stopifnot(identical(unname(unlist(reachable[columns[c(1, 4, 7)]])), c("A", "C", "D")),
          reachable$nearest_hospital_1_travel_distance_meters == 0)
few_body <- '{"code":"Ok","distances":[[null,50,null,null,100,null]],"durations":[[null,10,null,null,20,null]]}'
few <- fixed_run(few_body, on_error = "na")
stopifnot(few$nearest_hospital_1 == "B", few$nearest_hospital_2 == "E",
          all(is.na(few[columns[7:9]])))
must_error(fixed_run(few_body, on_error = "stop"), "Only 2 hospitals are reachable")
none <- fixed_run(
  '{"code":"Ok","distances":[[null,null,null,null,null,null]],"durations":[[null,null,null,null,null,null]]}',
  on_error = "na"
)
stopifnot(all(is.na(none[nearest_columns])),
          identical(unname(unlist(none[fixed_columns])), c(400, 5, 500, 1)))
for (body in c(
  '{"code":"NoTable"}',
  '{"code":"Ok","distances":[[10,20]],"durations":[[10,20]]}',
  '{"code":"Ok","distances":[[null,2,3,4,5,6]],"durations":[[1,2,3,4,5,6]]}',
  '{"code":"Ok","distances":[[-1,2,3,4,5,6]],"durations":[[1,2,3,4,5,6]]}',
  '{"code":"Ok","distances":[["1",2,3,4,5,6]],"durations":[[1,2,3,4,5,6]]}'
)) {
  must_error(fixed_run(body, on_error = "stop"), "OSRM")
  stopifnot(all(is.na(fixed_run(body, on_error = "na")[columns])))
}

# Exercise bundled data, the default 99-destination limit and a one-destination
# final batch. The mock routes all destinations at equal distance and duration.
batch_lengths <- integer()
constant_mock <- function(req) {
  destinations <- httr2::url_parse(req$url)$query$destinations
  size <- length(strsplit(destinations, ";", fixed = TRUE)[[1]])
  batch_lengths <<- c(batch_lengths, size)
  response_json(req, jsonlite::toJSON(list(code = "Ok",
    distances = list(as.list(rep(100, size))), durations = list(as.list(rep(10, size)))),
    auto_unbox = TRUE))
}
bundled <- httr2::with_mocked_responses(constant_mock,
  Moose_nearest_hospitals(one_location, delay = 0, progress = FALSE, on_error = "stop"))
stopifnot(identical(batch_lengths, c(99L, 13L)),
          bundled$nearest_hospital_1 == alberta_hospitals$hospital_name[1])
batch_lengths <- integer()
hundred <- httr2::with_mocked_responses(constant_mock,
  Moose_nearest_hospitals(one_location, hospitals = alberta_hospitals[1:100, ],
    delay = 0, progress = FALSE, on_error = "stop"))
stopifnot(identical(batch_lengths, c(99L, 1L)), ncol(hundred) == 15L)
batch_lengths <- integer()
bundled_ed <- httr2::with_mocked_responses(constant_mock,
  Moose_nearest_hospitals(one_location, emergency_only = TRUE,
    delay = 0, progress = FALSE, on_error = "stop"))
stopifnot(identical(batch_lengths, 94L), bundled_ed$nearest_hospital_1 %in%
            alberta_hospitals$hospital_name[alberta_hospitals$emergency_24h == 1L],
          identical(unname(unlist(bundled_ed[fixed_columns])), c(10, 100, 10, 100)))

# Fixed columns remain present when those hospitals are nearest, and reuse the
# existing routes rather than issuing duplicate requests.
requests <- list()
seen_candidates <- integer()
overlap <- httr2::with_mocked_responses(route_mock,
  Moose_nearest_hospitals(one_location, hospitals = mock_hospitals[c(1L, 7L, 8L), ],
    delay = 0, progress = FALSE, on_error = "stop"))
stopifnot(length(requests) == 1L, identical(seen_candidates, c(1L, 7L, 8L)),
          overlap$nearest_hospital_1 == "University of Alberta Hospital",
          overlap$nearest_hospital_2 == "Foothills Medical Centre",
          overlap[[fixed_columns[1]]] == overlap$nearest_hospital_2_travel_time_seconds,
          overlap[[fixed_columns[2]]] == overlap$nearest_hospital_2_travel_distance_meters,
          overlap[[fixed_columns[3]]] == overlap$nearest_hospital_1_travel_time_seconds,
          overlap[[fixed_columns[4]]] == overlap$nearest_hospital_1_travel_distance_meters)

# Matching names with altered coordinates do not override the fixed locations.
renamed <- hospitals
renamed$hospital_name[1:2] <- fixed_hospitals$hospital_name
changed_locations <- httr2::with_mocked_responses(route_mock,
  Moose_nearest_hospitals(one_location, hospitals = renamed,
    delay = 0, progress = FALSE, on_error = "stop"))
stopifnot(identical(unname(unlist(changed_locations[fixed_columns])), c(400, 5, 500, 1)))

# A failure in an unrelated candidate batch must not erase fixed results. A
# fixed-only failure likewise must not erase a complete nearest-three ranking.
fail_batch <- function(failed_batch) {
  batch <- 0L
  function(req) {
    batch <<- batch + 1L
    if (batch == failed_batch) return(response_json(req, '{"code":"TooBig"}', 400))
    route_mock(req)
  }
}
candidate_failed <- httr2::with_mocked_responses(fail_batch(2L),
  run(locations[1, ], on_error = "na"))
stopifnot(all(is.na(candidate_failed[nearest_columns])),
          identical(unname(unlist(candidate_failed[fixed_columns])), c(400, 5, 500, 1)))
fixed_failed <- httr2::with_mocked_responses(fail_batch(4L),
  run(locations[1, ], on_error = "na"))
stopifnot(identical(fixed_failed[nearest_columns], result[1, nearest_columns]),
          all(is.na(fixed_failed[fixed_columns])))
one_fixed_unreachable <- paste0('{"code":"Ok","distances":[[600,100,200,400,50,100,null,1]],',
                                '"durations":[[600,30,20,40,100,10,null,500]]}')
unreachable <- fixed_run(one_fixed_unreachable, on_error = "na")
stopifnot(all(is.na(unreachable[fixed_columns[1:2]])),
          identical(unname(unlist(unreachable[fixed_columns[3:4]])), c(500, 1)),
          identical(unname(unlist(unreachable[nearest_columns])),
                    unname(unlist(result[1, nearest_columns]))))
must_error(fixed_run(one_fixed_unreachable, on_error = "stop"),
           "No route to Foothills Medical Centre")
fixed_warning <- character()
warned_fixed <- withCallingHandlers(fixed_run(one_fixed_unreachable, on_error = "warn"),
  warning = function(w) {
    fixed_warning <<- c(fixed_warning, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
stopifnot(length(fixed_warning) == 1L,
          grepl("No route to Foothills Medical Centre", fixed_warning, fixed = TRUE))

# Authentication follows the same transport rules as Moose_travel().
requests <- list()
authenticated <- httr2::with_mocked_responses(route_mock,
  run(locations[1, ], username = "test-user", password = " test-password "))
request <- requests[[1L]]
printed <- paste(capture.output(print(request)), capture.output(str(request)), collapse = "\n")
token <- jsonlite::base64_enc(charToRaw("test-user: test-password "))
authorization <- get("wref_value", envir = asNamespace("httr2"), inherits = TRUE)(
  request$headers[["Authorization"]]
)
stopifnot(identical(request$options$followlocation, 0L),
          identical(authorization, paste("Basic", token)),
          !grepl("test-password", printed, fixed = TRUE),
          !grepl(token, printed, fixed = TRUE))

# Formatting changes only the five time columns, after numeric ranking. It
# covers the fixed destinations and does not introduce extra route requests.
time_columns <- columns[c(2L, 5L, 8L, 10L, 12L)]
other_columns <- setdiff(names(result), time_columns)
requests <- list()
formatted <- httr2::with_mocked_responses(route_mock,
  run(travel_time_format = TRUE, on_error = "stop"))
stopifnot(
  length(requests) == 8L,
  identical(names(formatted), names(result)),
  identical(formatted[other_columns], result[other_columns]),
  all(vapply(formatted[time_columns], is.character, logical(1))),
  identical(unname(unlist(formatted[1, time_columns])),
            c("00:01:40", "00:00:10", "00:00:30", "00:06:40", "00:08:20")),
  all(is.na(formatted[4:8, time_columns])),
  identical(unname(unlist(formatted[1, time_columns])),
            unname(unlist(formatted[3, time_columns])))
)
explicit_seconds <- httr2::with_mocked_responses(route_mock,
  run(travel_time_format = FALSE, on_error = "stop"))
stopifnot(identical(explicit_seconds, result))
reformatted <- httr2::with_mocked_responses(route_mock,
  run(result, overwrite = TRUE, travel_time_format = TRUE, on_error = "stop"))
stopifnot(identical(reformatted, formatted))

for (inputs in list(locations[FALSE, ], locations[4:8, ])) {
  no_routes <- httr2::with_mocked_responses(no_request,
    run(inputs, travel_time_format = TRUE, on_error = "stop"))
  stopifnot(nrow(no_routes) == nrow(inputs), all(is.na(no_routes[columns])),
            all(vapply(no_routes[time_columns], is.character, logical(1))))
}
for (flag in list(NA, "TRUE", 1, c(TRUE, FALSE), NULL)) {
  must_error(run(travel_time_format = flag), "`travel_time_format` must be TRUE or FALSE")
}

# The distance tie still uses fractional seconds, even if both formatted times
# are identical. Longer durations and rounding carry over correctly.
rounded <- fixed_run(paste0(
  '{"code":"Ok","distances":[[600,100,200,400,50,100,5,1]],',
  '"durations":[[600,59.49,20,40,90061,59.4,3599.5,86399.5]]}'
), travel_time_format = TRUE, on_error = "stop")
stopifnot(
  identical(unname(unlist(rounded[columns[c(1, 4, 7)]])), c("E", "F", "B")),
  identical(unname(unlist(rounded[time_columns])),
            c("25:01:01", "00:00:59", "00:00:59", "01:00:00", "24:00:00"))
)
formatted_failure <- httr2::with_mocked_responses(fail_batch(2L),
  run(locations[1, ], travel_time_format = TRUE, on_error = "na"))
stopifnot(all(is.na(formatted_failure[nearest_columns])),
          all(vapply(formatted_failure[time_columns], is.character, logical(1))),
          formatted_failure[[fixed_columns[1L]]] == "00:06:40",
          formatted_failure[[fixed_columns[3L]]] == "00:08:20")
formatted_unreachable <- fixed_run(one_fixed_unreachable,
  travel_time_format = TRUE, on_error = "na")
stopifnot(is.na(formatted_unreachable[[fixed_columns[1L]]]),
          is.character(formatted_unreachable[[fixed_columns[1L]]]),
          formatted_unreachable[[fixed_columns[3L]]] == "00:08:20")
