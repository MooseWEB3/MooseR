library(MooseR)

# Moose_nearest_hospitals_2() uses the suggested sf and osrm packages. These
# tests replace osrm::osrmTable() with a mock, so no routing server is needed.
test_nearest_hospitals_2 <- function() {
  real_table <- get("osrmTable", envir = asNamespace("osrm"))
  real_units <- get("moose_osrm_metres_per_unit", envir = asNamespace("MooseR"))
  on.exit({
    utils::assignInNamespace("osrmTable", real_table, ns = "osrm")
    utils::assignInNamespace("moose_osrm_metres_per_unit", real_units, ns = "MooseR")
  }, add = TRUE)

  must_error <- function(expr, pattern) {
    error <- tryCatch(force(expr), error = identity)
    stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error), fixed = TRUE))
    invisible(error)
  }
  capture_warnings <- function(expr) {
    warnings <- character()
    value <- withCallingHandlers(expr, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    list(value = value, warnings = warnings)
  }

  # Route distances deliberately disagree with geographic proximity, and the
  # nearest candidates include a distance tie decided by time. Times are
  # multiples of 6 seconds, the resolution of osrm's 0.1-minute durations.
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
  base_metres <- c(600, 100, 200, 400, 50, 100, 5, 1)
  base_seconds <- c(600, 30, 24, 42, 96, 12, 402, 498)
  locations <- data.frame(
    id = 1:8,
    lon = factor(c("-113.5", "-114", "-113.5", NA, "bad", "181", "-113", "Inf")),
    lat = c(53.5, 51, 53.5, 53, 53, 53, -91, 53)
  )

  # Mock osrmTable(): records each call and returns minutes and, like the
  # installed osrm version being emulated, kilometres or metres.
  calls <- list()
  use_km <- TRUE
  fail <- function(call_number, server) NULL
  transform <- function(table) table
  destination_index <- function(d) {
    index <- vapply(seq_len(nrow(d)), function(r) {
      hit <- which(abs(mock_hospitals$longitude - d[r, 1L]) < 1e-7 &
                     abs(mock_hospitals$latitude - d[r, 2L]) < 1e-7)
      if (length(hit) == 1L) hit else NA_integer_
    }, integer(1))
    stopifnot(!anyNA(index))
    index
  }
  # A second origin has different results, testing row alignment after reuse.
  scenario_basic <- function(s, d) {
    index <- destination_index(d)
    offset <- as.numeric(abs(s[, 1L] + 114) < 1e-6)
    list(metres = outer(offset * 1000, base_metres[index], "+"),
         seconds = outer(offset * 600, base_seconds[index], "+"))
  }
  scenario_values <- function(metres, seconds) {
    function(s, d) {
      index <- destination_index(d)
      list(metres = matrix(metres[index], nrow(s), length(index), byrow = TRUE),
           seconds = matrix(seconds[index], nrow(s), length(index), byrow = TRUE))
    }
  }
  scenario <- scenario_basic
  # Errors in the mock's own checks would otherwise look like request
  # failures, so they are also recorded and asserted empty at the end.
  mock_errors <- character()
  mock_table <- function(src, dst, loc, exclude, measure = "duration",
                         osrm.server = getOption("osrm.server"),
                         osrm.profile = getOption("osrm.profile")) {
    routes <- tryCatch({
      stopifnot(inherits(src, "sf"), inherits(dst, "sf"), missing(loc), missing(exclude),
                setequal(measure, c("duration", "distance")),
                sf::st_crs(src) == sf::st_crs(4326), sf::st_crs(dst) == sf::st_crs(4326))
      s <- sf::st_coordinates(src)
      d <- sf::st_coordinates(dst)
      calls[[length(calls) + 1L]] <<- list(server = osrm.server, profile = osrm.profile,
                                           src = s, dst = d)
      list(s = s, d = d, values = scenario(s, d))
    }, error = function(e) {
      mock_errors <<- c(mock_errors, conditionMessage(e))
      stop(e)
    })
    s <- routes$s
    d <- routes$d
    routes <- routes$values
    message("For all functions in the package, distances are expressed in kilometers.")
    failure <- fail(length(calls), osrm.server)
    if (!is.null(failure)) stop(failure, call. = FALSE)
    transform(list(
      durations = round(routes$seconds / 60, 1),
      distances = if (use_km) round(routes$metres / 1000, 3) else round(routes$metres),
      sources = data.frame(lon = s[, 1L], lat = s[, 2L]),
      destinations = data.frame(lon = d[, 1L], lat = d[, 2L])
    ))
  }
  set_units <- function(km) {
    use_km <<- km
    utils::assignInNamespace("moose_osrm_metres_per_unit",
                             function() if (km) 1000 else 1, ns = "MooseR")
  }
  reset <- function() {
    calls <<- list()
    fail <<- function(call_number, server) NULL
    transform <<- function(table) table
    scenario <<- scenario_basic
  }
  utils::assignInNamespace("osrmTable", mock_table, ns = "osrm")
  set_units(TRUE)

  run <- function(dataset = locations, ...) {
    Moose_nearest_hospitals_2(dataset, longitude = "lon", latitude = "lat",
                              hospitals = hospitals, osrm.server = "https://example.test",
                              osrm.profile = "driving", delay = 0, progress = FALSE, ...)
  }
  nearest_columns <- c(
    "nearest_hospital_1", "nearest_hospital_1_travel_time_seconds",
    "nearest_hospital_1_travel_distance_meters",
    "nearest_hospital_2", "nearest_hospital_2_travel_time_seconds",
    "nearest_hospital_2_travel_distance_meters",
    "nearest_hospital_3", "nearest_hospital_3_travel_time_seconds",
    "nearest_hospital_3_travel_distance_meters"
  )
  fixed_columns <- c(
    "foothills_medical_centre_travel_time_seconds",
    "foothills_medical_centre_travel_distance_meters",
    "university_of_alberta_hospital_travel_time_seconds",
    "university_of_alberta_hospital_travel_distance_meters"
  )
  columns <- c(nearest_columns, fixed_columns)
  values <- function(data, row, picked) {
    if (inherits(data, "sf")) data <- sf::st_drop_geometry(data)
    unname(unlist(data[row, picked]))
  }

  # Basic ranking, de-duplication and row alignment; osrm's unit note is hidden.
  reset()
  messages <- character()
  result <- withCallingHandlers(run(on_error = "stop"), message = function(m) {
    messages <<- c(messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  stopifnot(
    length(messages) == 0L,
    length(calls) == 1L, # Both unique valid origins share one request.
    nrow(calls[[1L]]$src) == 2L, nrow(calls[[1L]]$dst) == 8L,
    identical(calls[[1L]]$server, "https://example.test/"),
    identical(calls[[1L]]$profile, "driving"),
    identical(names(result), c(names(locations), columns)),
    identical(result[names(locations)], locations),
    identical(values(result, 1, columns[c(1, 4, 7)]), c("E", "F", "B")),
    identical(values(result, 1, columns[c(3, 6, 9)]), c(50, 100, 100)),
    identical(values(result, 1, columns[c(2, 5, 8)]), c(96, 12, 30)),
    identical(values(result, 2, columns[c(3, 6, 9)]), c(1050, 1100, 1100)),
    identical(values(result, 2, columns[c(2, 5, 8)]), c(696, 612, 630)),
    identical(values(result, 1, fixed_columns), c(402, 5, 498, 1)),
    identical(values(result, 2, fixed_columns), c(1002, 1005, 1098, 1001)),
    identical(values(result, 1, columns), values(result, 3, columns)),
    all(is.na(result[4:8, columns])),
    all(vapply(result[columns[c(1, 4, 7)]], is.character, logical(1))),
    all(vapply(result[setdiff(columns, columns[c(1, 4, 7)])], is.double, logical(1)))
  )

  # osrm before 6.0.0 reports metres; results are the same.
  reset()
  set_units(FALSE)
  metres_result <- run(on_error = "stop")
  set_units(TRUE)
  stopifnot(identical(metres_result, result))

  # Progress reports one message per batch of unique locations.
  reset()
  messages <- character()
  withCallingHandlers(
    Moose_nearest_hospitals_2(locations, longitude = "lon", latitude = "lat",
                              hospitals = hospitals, osrm.server = "https://example.test",
                              delay = 0, progress = TRUE),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  stopifnot(identical(messages, "Finding hospitals for locations 1-2 of 2...\n"))
  reset()
  messages <- character()
  withCallingHandlers(Moose_nearest_hospitals_2(data.frame(longitude = -113.5, latitude = 53.5),
                                                hospitals = hospitals, max_table_size = 1L,
                                                delay = 0, progress = TRUE),
                      message = function(m) {
                        messages <<- c(messages, conditionMessage(m))
                        invokeRestart("muffleMessage")
                      })
  stopifnot(identical(messages, "Finding hospitals for location 1 of 1...\n"))

  # max_table_size limits locations and location-hospital pairs per request.
  reset()
  small <- run(max_table_size = 2L, on_error = "stop")
  stopifnot(length(calls) == 4L,
            all(vapply(calls, function(call) nrow(call$src), integer(1)) == 1L),
            all(vapply(calls, function(call) nrow(call$dst), integer(1)) == 4L),
            identical(small, result))
  reset()
  stopifnot(identical(run(max_table_size = 3L, on_error = "stop"), result),
            length(calls) == 2L,
            identical(vapply(calls, function(call) nrow(call$dst), integer(1)), c(8L, 8L)))

  reset()
  filtered <- run(locations[1, ], emergency_only = TRUE, on_error = "stop")
  stopifnot(length(calls) == 1L, nrow(calls[[1L]]$dst) == 6L,
            identical(values(filtered, 1, columns[c(1, 4, 7)]), c("F", "B", "C")),
            identical(values(filtered, 1, fixed_columns), c(402, 5, 498, 1)))

  # Zero rows and invalid origins preserve the schema and make no requests.
  reset()
  empty <- run(locations[FALSE, ], on_error = "stop")
  invalid <- run(locations[4:8, ], on_error = "stop")
  stopifnot(length(calls) == 0L, nrow(empty) == 0L, identical(names(empty), names(result)),
            all(is.na(invalid[columns])))

  # Argument checks happen before any request.
  must_error(run(result), "Output columns already exist")
  fixed_conflict <- locations
  fixed_conflict[[fixed_columns[1]]] <- 999
  must_error(run(fixed_conflict), fixed_columns[1])
  stopifnot(identical(run(result, overwrite = TRUE), result))
  reset()
  one <- data.frame(longitude = -113.5, latitude = 53.5)
  must_error(Moose_nearest_hospitals_2(1:3), "data frame")
  must_error(Moose_nearest_hospitals_2(locations), "Unknown coordinate columns")
  must_error(Moose_nearest_hospitals_2(locations, longitude = c("id", "lon")), "longitude")
  must_error(run(emergency_only = NA), "emergency_only")
  must_error(Moose_nearest_hospitals_2(one, osrm.server = "https://user:secret@example.test"),
             "user information")
  must_error(Moose_nearest_hospitals_2(one, osrm.server = "https://example.test/?key=1"),
             "query string")
  must_error(Moose_nearest_hospitals_2(one, osrm.server = "https://example.test/a%20b"),
             "query string")
  must_error(Moose_nearest_hospitals_2(one, osrm.server = "ftp://example.test"), "valid HTTP(S)")
  must_error(Moose_nearest_hospitals_2(one, osrm.server = c("https://a.test", "https://b.test")),
             "`osrm.server`")
  must_error(Moose_nearest_hospitals_2(one, osrm.profile = "car/x"), "`osrm.profile`")
  for (size in list(1.5, 0, NA, "100", c(10, 20))) {
    must_error(Moose_nearest_hospitals_2(one, max_table_size = size), "max_table_size")
  }
  must_error(Moose_nearest_hospitals_2(one, delay = -1), "delay")
  must_error(Moose_nearest_hospitals_2(one, hospitals = hospitals[1:2, ]), "At least three")
  bad_hospitals <- hospitals
  bad_hospitals$latitude[1] <- NA
  must_error(Moose_nearest_hospitals_2(one, hospitals = bad_hospitals), "hospital coordinates")
  bad_hospitals <- hospitals
  bad_hospitals$emergency_24h[1] <- NA
  must_error(Moose_nearest_hospitals_2(one, hospitals = bad_hospitals, emergency_only = TRUE),
             "only 0 and 1")
  bad_hospitals <- hospitals
  bad_hospitals$hospital_name[2] <- "A"
  must_error(Moose_nearest_hospitals_2(one, hospitals = bad_hospitals), "unique")
  for (argument in c("engine", "max_candidates", "timeout", "batch_size", "server",
                     "profile", "username", "password")) {
    arguments <- list(one, 1)
    names(arguments) <- c("dataset", argument)
    must_error(do.call(Moose_nearest_hospitals_2, arguments), "unused argument")
  }
  stopifnot(length(calls) == 0L)

  # A failed candidate request must not produce an apparently complete
  # ranking, but fixed results from other requests are kept. Verify all three
  # error policies. With max_table_size = 1, each destination is one request.
  reset()
  fail <- function(call_number, server) {
    if (call_number == 3L) "OSRM API request failed [400]\nTooBig\nToo many table coordinates"
  }
  candidate_failed <- run(locations[1, ], max_table_size = 1L, on_error = "na")
  stopifnot(length(calls) == 8L, all(is.na(candidate_failed[nearest_columns])),
            identical(values(candidate_failed, 1, fixed_columns), c(402, 5, 498, 1)))
  reset()
  fail <- function(call_number, server) if (call_number == 8L) "Timeout was reached"
  fixed_failed <- run(locations[1, ], max_table_size = 1L, on_error = "na")
  stopifnot(identical(fixed_failed[nearest_columns], result[1, nearest_columns]),
            identical(values(fixed_failed, 1, fixed_columns), c(402, 5, NA, NA)))
  reset()
  fail <- function(call_number, server) if (call_number == 3L) "OSRM API request failed [400]"
  warned <- capture_warnings(run(locations[1, ], max_table_size = 1L, on_error = "warn"))
  stopifnot(length(warned$warnings) == 1L,
            grepl("1 unique location(s)", warned$warnings, fixed = TRUE),
            grepl("Input row 1: OSRM API request failed [400]", warned$warnings, fixed = TRUE),
            all(is.na(warned$value[nearest_columns])))
  reset()
  fail <- function(call_number, server) if (call_number == 3L) "OSRM API request failed [400]"
  must_error(run(locations[1, ], max_table_size = 1L, on_error = "stop"), "Input row 1:")
  # One failed request covers every location in it.
  reset()
  fail <- function(call_number, server) "OSRM API request failed [503]"
  shared <- capture_warnings(run(on_error = "warn"))
  stopifnot(length(calls) == 1L, length(shared$warnings) == 1L,
            grepl("2 unique location(s)", shared$warnings, fixed = TRUE),
            all(is.na(shared$value[columns])))
  reset()
  fail <- function(call_number, server) "OSRM API request failed [503]"
  must_error(run(on_error = "stop"),
             "Input row 1 (and 1 other location(s) in the same request): OSRM API request failed [503]")

  # NA cells mean unreachable destinations, not a request failure. Ties
  # (including co-located facilities) remain distinct and use candidate order.
  run_values <- function(metres, seconds, ...) {
    reset()
    scenario <<- scenario_values(metres, seconds)
    Moose_nearest_hospitals_2(one, hospitals = hospitals, osrm.server = "https://example.test",
                              delay = 0, progress = FALSE, ...)
  }
  reachable <- run_values(c(0, NA, 20, 20, NA, NA, 5, 1), c(0, NA, 12, 12, NA, NA, 402, 498),
                          on_error = "stop")
  stopifnot(identical(values(reachable, 1, columns[c(1, 4, 7)]), c("A", "C", "D")),
            reachable$nearest_hospital_1_travel_distance_meters == 0)
  few_metres <- c(NA, 50, NA, NA, 100, NA, 5, 1)
  few_seconds <- c(NA, 12, NA, NA, 18, NA, 402, 498)
  few <- run_values(few_metres, few_seconds, on_error = "na")
  stopifnot(few$nearest_hospital_1 == "B", few$nearest_hospital_2 == "E",
            all(is.na(few[columns[7:9]])))
  must_error(run_values(few_metres, few_seconds, on_error = "stop"),
             "Only 2 hospitals are reachable")
  none <- run_values(c(rep(NA, 6), 5, 1), c(rep(NA, 6), 402, 498), on_error = "na")
  stopifnot(all(is.na(none[nearest_columns])),
            identical(values(none, 1, fixed_columns), c(402, 5, 498, 1)))
  unreachable <- run_values(c(base_metres[1:6], NA, 1), c(base_seconds[1:6], NA, 498),
                            on_error = "na")
  stopifnot(all(is.na(unreachable[fixed_columns[1:2]])),
            identical(values(unreachable, 1, fixed_columns[3:4]), c(498, 1)),
            identical(values(unreachable, 1, nearest_columns), values(result, 1, nearest_columns)))
  must_error(run_values(c(base_metres[1:6], NA, 1), c(base_seconds[1:6], NA, 498),
                        on_error = "stop"), "No route to Foothills Medical Centre")
  fixed_warning <- capture_warnings(run_values(c(base_metres[1:6], NA, 1),
                                               c(base_seconds[1:6], NA, 498), on_error = "warn"))
  stopifnot(length(fixed_warning$warnings) == 1L,
            grepl("No route to Foothills Medical Centre", fixed_warning$warnings, fixed = TRUE))

  # Invalid osrm output is a request failure.
  bad_outputs <- list(
    function(table) { table$durations <- NULL; table },
    function(table) { table$distances <- table$distances[, -1L, drop = FALSE]; table },
    function(table) { table$distances[1L, 1L] <- -1; table },
    function(table) { table$durations[] <- "1"; table },
    function(table) { table$distances[1L, 1L] <- NA; table },
    function(table) "not a table"
  )
  for (bad in bad_outputs) {
    reset()
    transform <- bad
    must_error(run(locations[1, ], on_error = "stop"), "osrm returned")
    reset()
    transform <- bad
    stopifnot(all(is.na(run(locations[1, ], on_error = "na")[columns])))
  }

  # Bundled data: all 112 hospitals in one request per 89 locations, or 94
  # emergency hospitals (including both fixed ones) per 100 locations.
  constant <- function(s, d) {
    list(metres = matrix(100, nrow(s), nrow(d)), seconds = matrix(60, nrow(s), nrow(d)))
  }
  many <- data.frame(longitude = -113.5 - (1:200) / 1000, latitude = 53.5)
  reset()
  scenario <- constant
  bundled <- Moose_nearest_hospitals_2(one, delay = 0, progress = FALSE, on_error = "stop")
  stopifnot(length(calls) == 1L, nrow(calls[[1L]]$dst) == nrow(alberta_hospitals),
            nrow(alberta_hospitals) == 112L,
            bundled$nearest_hospital_1 == alberta_hospitals$hospital_name[1])
  reset()
  scenario <- constant
  bundled_many <- Moose_nearest_hospitals_2(many, delay = 0, progress = FALSE, on_error = "stop")
  stopifnot(identical(vapply(calls, function(call) nrow(call$src), integer(1)), c(89L, 89L, 22L)),
            all(bundled_many$nearest_hospital_1 == alberta_hospitals$hospital_name[1]))
  reset()
  scenario <- constant
  bundled_ed <- Moose_nearest_hospitals_2(many, emergency_only = TRUE, delay = 0,
                                          progress = FALSE, on_error = "stop")
  stopifnot(identical(vapply(calls, function(call) nrow(call$src), integer(1)), c(100L, 100L)),
            all(vapply(calls, function(call) nrow(call$dst), integer(1)) == 94L),
            all(bundled_ed$nearest_hospital_1 %in%
                  alberta_hospitals$hospital_name[alberta_hospitals$emergency_24h == 1L]),
            identical(values(bundled_ed, 1, fixed_columns), c(60, 100, 60, 100)))

  # Fixed columns remain present when those hospitals are nearest, and reuse
  # the existing routes rather than adding destinations.
  reset()
  overlap <- Moose_nearest_hospitals_2(one, hospitals = mock_hospitals[c(1L, 7L, 8L), ],
                                       delay = 0, progress = FALSE, on_error = "stop")
  stopifnot(length(calls) == 1L, nrow(calls[[1L]]$dst) == 3L,
            overlap$nearest_hospital_1 == "University of Alberta Hospital",
            overlap$nearest_hospital_2 == "Foothills Medical Centre",
            overlap[[fixed_columns[1]]] == overlap$nearest_hospital_2_travel_time_seconds,
            overlap[[fixed_columns[2]]] == overlap$nearest_hospital_2_travel_distance_meters,
            overlap[[fixed_columns[3]]] == overlap$nearest_hospital_1_travel_time_seconds,
            overlap[[fixed_columns[4]]] == overlap$nearest_hospital_1_travel_distance_meters)
  # Matching names with altered coordinates do not override the fixed locations.
  renamed <- hospitals
  renamed$hospital_name[1:2] <- fixed_hospitals$hospital_name
  reset()
  changed <- Moose_nearest_hospitals_2(one, hospitals = renamed, delay = 0,
                                       progress = FALSE, on_error = "stop")
  stopifnot(identical(values(changed, 1, fixed_columns), c(402, 5, 498, 1)))

  # sf input: geometry in any CRS replaces the coordinate columns, and the
  # result stays an sf object with its geometry.
  points <- sf::st_as_sf(data.frame(id = 1:3, lon = c(-113.5, -114, -113.5), lat = c(53.5, 51, 53.5)),
                         coords = c("lon", "lat"), crs = 4326)
  projected <- sf::st_transform(points, 3857)
  reset()
  sf_result <- Moose_nearest_hospitals_2(projected, hospitals = hospitals,
                                         osrm.server = "https://example.test", delay = 0,
                                         progress = FALSE, on_error = "stop")
  stopifnot(inherits(sf_result, "sf"), length(calls) == 1L, nrow(calls[[1L]]$src) == 2L,
            identical(names(sf_result), c(names(projected), columns)),
            identical(sf::st_geometry(sf_result), sf::st_geometry(projected)),
            identical(sf_result$id, 1:3))
  for (column in columns) {
    stopifnot(identical(sf_result[[column]], result[[column]][1:3]))
  }
  must_error(Moose_nearest_hospitals_2(projected, longitude = "id"), "cannot be used")
  with_empty <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(
    sf::st_point(c(-113.5, 53.5)), sf::st_point(), crs = 4326
  ))
  reset()
  empty_point <- Moose_nearest_hospitals_2(with_empty, hospitals = hospitals,
                                           osrm.server = "https://example.test", delay = 0,
                                           progress = FALSE, on_error = "stop")
  stopifnot(identical(values(empty_point, 1, columns[c(1, 4, 7)]), c("E", "F", "B")),
            all(is.na(sf::st_drop_geometry(empty_point)[2, columns])))
  no_crs <- sf::st_as_sf(data.frame(x = -113.5, y = 53.5), coords = c("x", "y"))
  must_error(Moose_nearest_hospitals_2(no_crs), "coordinate reference system")
  line <- sf::st_sf(id = 1, geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(-113.5, -113.4, 53.5, 53.6), 2)), crs = 4326
  ))
  must_error(Moose_nearest_hospitals_2(line), "POINT")
  hospitals_sf <- sf::st_transform(
    sf::st_as_sf(hospitals, coords = c("longitude", "latitude"), crs = 4326), 3857
  )
  run_sf_hospitals <- function(...) {
    Moose_nearest_hospitals_2(one, hospitals = hospitals_sf, osrm.server = "https://example.test",
                              delay = 0, progress = FALSE, on_error = "stop", ...)
  }
  reset()
  sf_hospitals <- run_sf_hospitals()
  stopifnot(identical(values(sf_hospitals, 1, columns), values(result, 1, columns)))
  reset()
  sf_emergency <- run_sf_hospitals(emergency_only = TRUE)
  stopifnot(identical(values(sf_emergency, 1, columns), values(filtered, 1, columns)))

  # By default, routing uses osrm's own server options: the public
  # routing.openstreetmap.de server and its "car" profile.
  default_options <- function() {
    Moose_nearest_hospitals_2(one, hospitals = hospitals, delay = 0, progress = FALSE)
    calls[[1L]][c("server", "profile")]
  }
  old_options <- options(osrm.server = NULL, osrm.profile = NULL)
  on.exit(options(old_options), add = TRUE)
  reset()
  fallback <- default_options()
  options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "car")
  reset()
  from_options <- default_options()
  options(osrm.server = "https://osrm.example.test///", osrm.profile = "bike")
  reset()
  changed_options <- default_options()
  options(old_options)
  stopifnot(identical(fallback, list(server = "https://routing.openstreetmap.de/",
                                     profile = "car")),
            identical(from_options, fallback),
            identical(changed_options, list(server = "https://osrm.example.test/",
                                            profile = "bike")))
  # The URL handed to osrm ends in one slash, since osrm appends "table/v1/...".
  reset()
  Moose_nearest_hospitals_2(one, hospitals = hospitals,
                            osrm.server = " https://routing.openstreetmap.de// ",
                            osrm.profile = " foot ", delay = 0, progress = FALSE)
  stopifnot(identical(calls[[1L]]$server, "https://routing.openstreetmap.de/"),
            identical(calls[[1L]]$profile, "foot"))
  # osrm recognizes the public server only by its exact URL, so other
  # spellings are normalized, and profiles it lacks stop before any request.
  reset()
  Moose_nearest_hospitals_2(one, hospitals = hospitals,
                            osrm.server = "HTTP://Routing.OpenStreetMap.de",
                            osrm.profile = "bike", delay = 0, progress = FALSE)
  stopifnot(identical(calls[[1L]]$server, "https://routing.openstreetmap.de/"))
  reset()
  must_error(Moose_nearest_hospitals_2(one, osrm.server = "https://routing.openstreetmap.de/",
                                       osrm.profile = "driving"), "offers the profiles")
  stopifnot(length(calls) == 0L)

  # `delay` spaces requests across calls as well as within one.
  pace <- get(".moose_osrm_pace", envir = asNamespace("MooseR"))
  pace$last <- as.numeric(Sys.time())
  started <- proc.time()[["elapsed"]]
  MooseR:::moose_osrm_wait(0.3)
  stopifnot(proc.time()[["elapsed"]] - started >= 0.2)
  started <- proc.time()[["elapsed"]]
  MooseR:::moose_osrm_wait(0)
  stopifnot(proc.time()[["elapsed"]] - started < 0.2)

  # Options set before osrm is loaded survive its loading, which would
  # otherwise reset them. This needs an R process where osrm is not loaded.
  expression <- paste0(
    ".libPaths(", paste(deparse(.libPaths()), collapse = ""), "); ",
    "options(osrm.server = \"https://osrm.example.test/\", osrm.profile = \"bike\"); ",
    "suppressMessages(library(MooseR)); ",
    "invisible(Moose_nearest_hospitals_2(data.frame(longitude = numeric(), ",
    "latitude = numeric()), progress = FALSE)); ",
    "cat(isNamespaceLoaded(\"osrm\"), getOption(\"osrm.server\"), getOption(\"osrm.profile\"))"
  )
  kept <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(expression)),
                  stdout = TRUE)
  stopifnot(identical(kept, "TRUE https://osrm.example.test/ bike"))

  # Formatting changes only the five time columns, after numeric ranking.
  time_columns <- columns[c(2L, 5L, 8L, 10L, 12L)]
  other_columns <- setdiff(names(result), time_columns)
  reset()
  formatted <- run(travel_time_format = TRUE, on_error = "stop")
  stopifnot(
    length(calls) == 1L,
    identical(names(formatted), names(result)),
    identical(formatted[other_columns], result[other_columns]),
    all(vapply(formatted[time_columns], is.character, logical(1))),
    identical(values(formatted, 1, time_columns),
              c("00:01:36", "00:00:12", "00:00:30", "00:06:42", "00:08:18")),
    all(is.na(formatted[4:8, time_columns]))
  )
  reset()
  stopifnot(identical(run(result, overwrite = TRUE, travel_time_format = TRUE,
                          on_error = "stop"), formatted))
  reset()
  no_routes <- run(locations[4:8, ], travel_time_format = TRUE, on_error = "stop")
  stopifnot(length(calls) == 0L, all(is.na(no_routes[columns])),
            all(vapply(no_routes[time_columns], is.character, logical(1))))
  for (flag in list(NA, "TRUE", 1, c(TRUE, FALSE), NULL)) {
    must_error(run(travel_time_format = flag), "`travel_time_format` must be TRUE or FALSE")
  }

  # Unit conversion follows the installed osrm version.
  stopifnot(identical(real_units(),
                      if (utils::packageVersion("osrm") >= "6.0.0") 1000 else 1),
            isTRUE(MooseR:::moose_require_sf_osrm()))

  # The mock's own checks never failed, even where errors became NA results.
  if (length(mock_errors)) {
    stop("The osrmTable() mock failed: ", paste(unique(mock_errors), collapse = "; "))
  }
  invisible(TRUE)
}

if (isTRUE(tryCatch(MooseR:::moose_require_sf_osrm(), error = function(e) FALSE))) {
  test_nearest_hospitals_2()
} else {
  message("Skipping Moose_nearest_hospitals_2() tests: install sf (>= 1.0-0) and osrm (>= 4.0.0).")
}
