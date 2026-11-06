#' Add the three nearest hospitals and fixed-hospital routes with sf and osrm
#'
#' A second implementation of [Moose_nearest_hospitals()], built on the
#' \pkg{sf} and \pkg{osrm} packages. It appends the same 13 columns and picks
#' hospitals by the same rules: for each location, the three nearest eligible
#' hospitals by road-route distance, plus travel time and distance to
#' Foothills Medical Centre and University of Alberta Hospital.
#' [Moose_nearest_hospitals()] itself is unchanged.
#'
#' Locations and hospitals become \pkg{sf} POINT features in WGS 84
#' (EPSG:4326), and road routes come from [osrm::osrmTable()], which queries
#' the OSRM Table service for durations and distances. `dataset` and
#' `hospitals` can also be \pkg{sf} POINT objects in any coordinate reference
#' system: their geometry is transformed to WGS 84 with [sf::st_transform()].
#'
#' Routing uses the \pkg{osrm} package's own server settings, the
#' `osrm.server` and `osrm.profile` options. By default these are the public
#' FOSSGIS server `https://routing.openstreetmap.de/` and its `"car"` profile,
#' so no MooseR or private server is involved. That server allows at most one
#' request per second and no heavy use, so requests are spaced at least `delay`
#' seconds apart, also across calls; use it for light, interactive work.
#' Coordinates are sent to the server and may appear in its request logs. If
#' you set these options yourself before \pkg{osrm} is loaded, which would
#' normally overwrite them, your settings are kept. Routes are computed from
#' OpenStreetMap data, (c) OpenStreetMap contributors, under the Open Database
#' License; the default server is run by FOSSGIS e.V.
#'
#' Ranking and output rules match [Moose_nearest_hospitals()] with its default
#' OSRM engine. Hospitals are ranked by increasing route distance, then travel
#' time, then their row order in `hospitals`; every eligible hospital is
#' compared, with no straight-line prefilter. OSRM returns fastest routes, so
#' distances are not guaranteed to be the shortest possible road paths, and
#' times are estimates without live traffic. The two fixed hospitals always use
#' their coordinates in [alberta_hospitals] and never enter a custom
#' nearest-three candidate list. Repeated input coordinates are routed once,
#' and fixed-hospital routes reuse candidate routes with the same coordinates.
#' Unreachable candidates are excluded; if fewer than three are reachable, the
#' remaining ranks are `NA`. A failed request leaves all nine nearest-hospital
#' results of its locations `NA`, because an incomplete comparison cannot
#' establish the nearest hospitals; fixed-hospital results from successful
#' requests are kept. A failed or unreachable fixed destination has `NA` in its
#' two columns.
#'
#' Differences from [Moose_nearest_hospitals()]:
#' * The default server differs: [Moose_nearest_hospitals()] uses
#'   `https://router.project-osrm.org` with profile `"driving"`, so default
#'   results can differ by more than rounding. `Moose_nearest_hospitals(server
#'   = "https://routing.openstreetmap.de/routed-car", profile = "driving")`
#'   uses the same server as this function's default.
#' * One request routes many locations at once: up to `max_table_size`
#'   locations, and at most `max_table_size^2` location-hospital pairs. With
#'   the 112 bundled hospitals and the default of 100, that is 89 locations per
#'   request, instead of two requests per location. A failed request affects
#'   every location in it.
#' * \pkg{osrm} reports durations in minutes rounded to 0.1 minute, so travel
#'   times are multiples of 6 seconds, and distances are rounded to whole
#'   metres. \pkg{osrm} also sends coordinates with five decimal places (about
#'   1 m). Results can therefore differ slightly from
#'   [Moose_nearest_hospitals()], and hospitals whose route distances differ by
#'   less than a metre can swap ranks.
#' * \pkg{osrm} 6.0.0 and later report distances in kilometres and earlier
#'   versions in metres; both are converted to metres.
#' * Only OSRM through \pkg{osrm} is supported: there is no `engine`,
#'   `max_candidates`, `timeout`, `username` or `password` argument;
#'   `osrm.server` and `osrm.profile` replace `server` and `profile`, and
#'   `max_table_size` replaces `batch_size`. \pkg{osrm} has no timeout or
#'   redirect settings, so the defaults of the \pkg{curl} package apply.
#'   Messages printed by \pkg{osrm}, such as its note on units, are
#'   suppressed. With `on_error = "stop"`, a failed request names its first
#'   input row and how many other locations it held.
#'
#' Requires the \pkg{sf} (>= 1.0-0) and \pkg{osrm} (>= 4.0.0) packages, which
#' are suggested rather than required by MooseR. Install them with
#' `install.packages(c("sf", "osrm"))`. Current releases need R 4.1.0 or later
#' (\pkg{sf}) and R 3.5.0 or later (\pkg{osrm}), so both install on R 4.3.0,
#' and the code uses only functions available in R 4.3.0 and in those minimum
#' package versions.
#'
#' @param dataset A data frame, tibble or \pkg{sf} object with POINT geometry.
#'   Original rows and columns, and the \pkg{sf} class and geometry, are
#'   retained. Missing, empty, non-numeric, non-finite or out-of-range
#'   coordinates return 13 `NA` values without being routed.
#' @param longitude,latitude Names of columns containing origin coordinates in
#'   decimal degrees. Numeric strings and factors are accepted. Not used, and
#'   not allowed, when `dataset` is an \pkg{sf} object: coordinates then come
#'   from its geometry.
#' @param hospitals Optional candidate hospitals: a data frame with unique,
#'   non-empty `hospital_name` values and valid `longitude` and `latitude`
#'   columns, or an \pkg{sf} POINT object with a `hospital_name` column. `NULL`
#'   uses the bundled [alberta_hospitals]. At least three eligible rows are
#'   required. Other columns are ignored except `emergency_24h` when
#'   filtering. This argument controls only the nearest-three candidates, not
#'   the two fixed destinations.
#' @param emergency_only Logical. If `TRUE`, restrict nearest-three candidates
#'   to rows with `emergency_24h == 1`. This flag describes regular 24-hour
#'   emergency service, not temporary closures or current availability.
#' @param osrm.server,osrm.profile Passed to [osrm::osrmTable()]. The defaults
#'   are the \pkg{osrm} package's own options: the public server
#'   `https://routing.openstreetmap.de/` with profile `"car"`; `"bike"` and
#'   `"foot"` are its only other profiles. `osrm.server` must be an HTTP(S) URL
#'   without user information, a query string, a fragment or a percent sign.
#' @param max_table_size Largest table the server accepts: 100 for OSRM's own
#'   default (`osrm-routed --max-table-size`), which matches the 10,000 pairs
#'   per request that \pkg{osrm} documents for the public server. A whole
#'   number of at least 1. Each request has at most `max_table_size`
#'   locations and at most `max_table_size^2` location-hospital pairs. Use the
#'   default unless you know the server's limit; a smaller value puts fewer
#'   locations in each request.
#' @param overwrite Logical. Replace existing output columns if `TRUE`.
#' @param on_error One of `"warn"`, `"stop"`, or `"na"`. Controls request
#'   failures, unreachable fixed hospitals and fewer than three reachable
#'   candidates. `"warn"` gives a summary warning; `"na"` returns the available
#'   results silently. Invalid input coordinates always return `NA`,
#'   regardless of this setting.
#' @param delay Non-negative minimum seconds between requests, including the
#'   last request of a previous call. The default of 1 follows the default
#'   server's limit of one request per second.
#' @param progress Logical. Print progress for each batch of unique
#'   locations.
#' @param travel_time_format Logical. Defaults to `FALSE`, returning numeric
#'   seconds. If `TRUE`, format all five travel-time columns as `HH:MM:SS`
#'   character strings with [Moose_second2format()]. Hours can exceed 24 and
#'   missing values remain `NA`. Column names (including `_seconds`) and the
#'   13-column output schema are retained. Ranking uses the numeric times.
#'
#' @return `dataset` (still an \pkg{sf} object if it was one) with the same
#'   13 columns as [Moose_nearest_hospitals()]. The first nine are, for each
#'   `i` in 1, 2, 3, `nearest_hospital_i` (character),
#'   `nearest_hospital_i_travel_time_seconds` (numeric) and
#'   `nearest_hospital_i_travel_distance_meters` (numeric). The four additional
#'   numeric columns are `foothills_medical_centre_travel_time_seconds`,
#'   `foothills_medical_centre_travel_distance_meters`,
#'   `university_of_alberta_hospital_travel_time_seconds` and
#'   `university_of_alberta_hospital_travel_distance_meters`. These are
#'   included even when either fixed hospital is also among the nearest three.
#'   With `travel_time_format = TRUE`, all five travel-time columns contain
#'   character strings instead of numeric seconds; distance columns stay
#'   numeric.
#' @seealso [Moose_nearest_hospitals()], [alberta_hospitals],
#'   [Moose_second2format()], [osrm::osrmTable()]
#' @references \url{https://project-osrm.org/docs/v5.24.0/api/#table-service},
#'   \url{https://routing.openstreetmap.de/about.html}
#' @examples
#' \dontrun{
#' locations <- data.frame(longitude = c(-113.4938, -114.0719),
#'                         latitude = c(53.5461, 51.0447))
#' result <- Moose_nearest_hospitals_2(locations)
#' emergency_result <- Moose_nearest_hospitals_2(locations, emergency_only = TRUE)
#' formatted_result <- Moose_nearest_hospitals_2(locations, travel_time_format = TRUE)
#'
#' # sf points in any coordinate reference system, here NAD83 / Alberta 10-TM:
#' points <- sf::st_as_sf(locations, coords = c("longitude", "latitude"), crs = 4326)
#' points <- sf::st_transform(points, 3400)
#' result_sf <- Moose_nearest_hospitals_2(points)
#' }
#' @export
Moose_nearest_hospitals_2 <- function(dataset,
                                      longitude = "longitude",
                                      latitude = "latitude",
                                      hospitals = NULL,
                                      emergency_only = FALSE,
                                      osrm.server = getOption("osrm.server"),
                                      osrm.profile = getOption("osrm.profile"),
                                      max_table_size = 100L,
                                      overwrite = FALSE,
                                      on_error = c("warn", "stop", "na"),
                                      delay = 1,
                                      progress = interactive(),
                                      travel_time_format = FALSE) {
  # Loading osrm sets its osrm.server and osrm.profile options, overwriting any
  # the user set beforehand; keep the user's settings.
  user_options <- if (!isNamespaceLoaded("osrm")) {
    Filter(Negate(is.null), list(osrm.server = getOption("osrm.server"),
                                 osrm.profile = getOption("osrm.profile")))
  }
  moose_require_sf_osrm()
  if (length(user_options)) options(user_options)
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
  validate_number <- function(x, argument, minimum) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < minimum) {
      stop("`", argument, "` must be a finite number >= ", minimum, ".", call. = FALSE)
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
    stop("`dataset` must be a data frame, tibble or sf object with unique column names.",
         call. = FALSE)
  }
  dataset_is_sf <- inherits(dataset, "sf")
  if (dataset_is_sf) {
    if (!missing(longitude) || !missing(latitude)) {
      stop("`longitude` and `latitude` cannot be used when `dataset` is an sf object; ",
           "coordinates are read from its geometry.", call. = FALSE)
    }
  } else {
    longitude <- validate_text(longitude, "longitude")
    latitude <- validate_text(latitude, "latitude")
    missing_columns <- setdiff(c(longitude, latitude), names(dataset))
    if (length(missing_columns)) {
      stop("Unknown coordinate columns: ", paste(missing_columns, collapse = ", "), ".",
           call. = FALSE)
    }
  }
  validate_flag(emergency_only, "emergency_only")
  validate_flag(overwrite, "overwrite")
  validate_flag(progress, "progress")
  validate_flag(travel_time_format, "travel_time_format")
  on_error <- match.arg(on_error)
  validate_number(delay, "delay", 0)
  validate_number(max_table_size, "max_table_size", 1)
  if (max_table_size != floor(max_table_size)) {
    stop("`max_table_size` must be a whole number of at least 1.", call. = FALSE)
  }
  # The same defaults osrm sets for these options when it loads.
  if (is.null(osrm.server)) osrm.server <- "https://routing.openstreetmap.de/"
  if (is.null(osrm.profile)) osrm.profile <- "car"
  osrm.profile <- trimws(validate_text(osrm.profile, "osrm.profile"))
  if (!grepl("^[A-Za-z0-9._~-]+\\z", osrm.profile, perl = TRUE)) {
    stop("`osrm.profile` can contain only letters, digits, `.`, `_`, `~` and `-`.",
         call. = FALSE)
  }
  osrm.server <- trimws(validate_text(osrm.server, "osrm.server"))
  # osrm escapes its request URL only when the URL has no percent-escape yet,
  # so a "%" in the server URL would leave the encoded coordinates unescaped.
  if (grepl("[?#%]", osrm.server)) {
    stop("`osrm.server` must not contain a query string, fragment or `%`.", call. = FALSE)
  }
  if (grepl("^[^/]*//[^/]*@", osrm.server)) {
    stop("`osrm.server` must not contain user information such as `user:password@`.",
         call. = FALSE)
  }
  if (!grepl("^https?://[^/[:space:]]+(/[^[:space:]]*)?$", osrm.server, ignore.case = TRUE)) {
    stop("`osrm.server` must be a valid HTTP(S) URL, such as ",
         "https://routing.openstreetmap.de/.", call. = FALSE)
  }
  # osrm appends "table/v1/<profile>/" to the server URL, so it needs one
  # trailing slash. osrm recognizes the public server, which has its own URL
  # form and profiles, only by its exact URL.
  osrm.server <- sub("/+$", "", osrm.server)
  if (grepl("^https?://routing[.]openstreetmap[.]de$", osrm.server, ignore.case = TRUE)) {
    osrm.server <- "https://routing.openstreetmap.de"
    if (!osrm.profile %in% c("car", "bike", "foot")) {
      stop("routing.openstreetmap.de offers the profiles \"car\", \"bike\" and \"foot\", ",
           "not \"", osrm.profile, "\".", call. = FALSE)
    }
  }
  osrm.server <- paste0(osrm.server, "/")

  output_names <- unlist(lapply(1:3, function(i) {
    paste0("nearest_hospital_", i, c("", "_travel_time_seconds", "_travel_distance_meters"))
  }), use.names = FALSE)
  fixed_columns <- c(
    "foothills_medical_centre_travel_time_seconds",
    "foothills_medical_centre_travel_distance_meters",
    "university_of_alberta_hospital_travel_time_seconds",
    "university_of_alberta_hospital_travel_distance_meters"
  )
  output_names <- c(output_names, fixed_columns)
  time_columns <- output_names[c(2L, 5L, 8L, 10L, 12L)]
  conflicts <- intersect(output_names, names(dataset))
  if (length(conflicts) && !overwrite) {
    stop("Output columns already exist: ", paste(conflicts, collapse = ", "),
         ". Use `overwrite = TRUE` to replace them.", call. = FALSE)
  }

  bundled <- new.env(parent = emptyenv())
  utils::data("alberta_hospitals", package = "MooseR", envir = bundled)
  fixed_names <- c("Foothills Medical Centre", "University of Alberta Hospital")
  fixed_ids <- match(fixed_names, bundled$alberta_hospitals$hospital_name)
  if (anyNA(fixed_ids)) {
    stop("The bundled hospital data are missing a fixed destination.", call. = FALSE)
  }
  fixed_hospitals <- bundled$alberta_hospitals[fixed_ids, , drop = FALSE]
  if (is.null(hospitals)) hospitals <- bundled$alberta_hospitals
  hospitals_is_sf <- inherits(hospitals, "sf")
  required <- c("hospital_name", if (!hospitals_is_sf) c("longitude", "latitude"),
                if (emergency_only) "emergency_24h")
  if (!is.data.frame(hospitals) || anyDuplicated(names(hospitals)) ||
      !all(required %in% names(hospitals))) {
    stop("`hospitals` must be a data frame with columns: ",
         paste(required, collapse = ", "),
         if (!hospitals_is_sf) {
           "; or an sf POINT object with a hospital_name column"
         }, ".", call. = FALSE)
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
  if (hospitals_is_sf) {
    hospital_points <- moose_sf_lonlat(hospitals, "hospitals")
    hospital_lon <- hospital_points$lon
    hospital_lat <- hospital_points$lat
  } else {
    hospital_lon <- as_coordinate(hospitals$longitude)
    hospital_lat <- as_coordinate(hospitals$latitude)
  }
  if (!all(valid_coordinates(hospital_lon, hospital_lat))) {
    stop("Eligible hospital coordinates must be valid decimal degrees.", call. = FALSE)
  }

  # Reuse a candidate's route only for the same coordinates. A custom list may
  # give the same hospital name different coordinates; fixed outputs always use
  # the bundled locations and never expand the nearest-three candidate set.
  coordinate_key <- function(lon, lat) sprintf("%.17g|%.17g", lon, lat)
  fixed_destinations <- match(
    coordinate_key(fixed_hospitals$longitude, fixed_hospitals$latitude),
    coordinate_key(hospital_lon, hospital_lat)
  )
  extra <- which(is.na(fixed_destinations))
  fixed_destinations[extra] <- length(hospital_lon) + seq_along(extra)
  destination_lon <- c(hospital_lon, fixed_hospitals$longitude[extra])
  destination_lat <- c(hospital_lat, fixed_hospitals$latitude[extra])

  if (dataset_is_sf) {
    origin_points <- moose_sf_lonlat(dataset, "dataset")
    origin_lon <- origin_points$lon
    origin_lat <- origin_points$lat
  } else {
    origin_lon <- as_coordinate(dataset[[longitude]])
    origin_lat <- as_coordinate(dataset[[latitude]])
  }
  valid_rows <- which(valid_coordinates(origin_lon, origin_lat))
  keys <- coordinate_key(origin_lon[valid_rows], origin_lat[valid_rows])
  unique_positions <- which(!duplicated(keys))
  unique_rows <- valid_rows[unique_positions]
  group <- match(keys, keys[unique_positions])
  n_unique <- length(unique_rows)

  # Results are collected once per unique location and expanded to rows at
  # the end, so large data sets are not copied once per location.
  nearest_name <- matrix(NA_character_, n_unique, 3L)
  nearest_time <- nearest_distance <- matrix(NA_real_, n_unique, 3L)
  fixed_time <- fixed_distance <- matrix(NA_real_, n_unique, 2L)
  failure_count <- 0L
  first_failure <- NULL

  if (n_unique) {
    # Up to `max_table_size` locations and `max_table_size^2` pairs per request.
    n_destinations <- length(destination_lon)
    pairs <- max_table_size^2
    destination_size <- min(n_destinations, pairs)
    destination_batches <- split(seq_len(n_destinations),
                                 ceiling(seq_len(n_destinations) / destination_size))
    origin_size <- max(1, min(max_table_size, floor(pairs / destination_size)))
    origin_batches <- split(seq_len(n_unique), ceiling(seq_len(n_unique) / origin_size))
    origin_sf <- moose_sf_points(origin_lon[unique_rows], origin_lat[unique_rows])
    destination_sf <- moose_sf_points(destination_lon, destination_lat)
    candidates <- seq_len(nrow(hospitals))

    for (batch in origin_batches) {
      size <- length(batch)
      if (progress) {
        which_locations <- if (size > 1L) {
          paste0("locations ", batch[1L], "-", batch[size])
        } else {
          paste("location", batch[1L])
        }
        message("Finding hospitals for ", which_locations, " of ", n_unique, "...")
      }
      distances <- durations <- matrix(NA_real_, size, n_destinations)
      queried <- rep(FALSE, n_destinations)
      issues <- character()
      for (destinations in destination_batches) {
        moose_osrm_wait(delay)
        table <- tryCatch(
          moose_osrm_pkg_table(origin_sf[batch, ], destination_sf[destinations, ],
                               osrm.server, osrm.profile),
          error = identity
        )
        if (inherits(table, "error")) {
          issue <- conditionMessage(table)
          if (on_error == "stop") {
            stop("Input row ", unique_rows[batch[1L]],
                 if (size > 1L) {
                   paste0(" (and ", size - 1L, " other location(s) in the same request)")
                 }, ": ", issue, call. = FALSE)
          }
          issues <- c(issues, issue)
        } else {
          distances[, destinations] <- table$distance
          durations[, destinations] <- table$duration
          queried[destinations] <- TRUE
        }
      }

      complete <- all(queried[candidates])
      for (k in seq_len(size)) {
        i <- batch[k]
        distance <- distances[k, ]
        duration <- durations[k, ]
        location_issues <- issues
        if (complete) {
          reachable <- candidates[!is.na(distance[candidates])]
          ranked <- reachable[order(distance[reachable], duration[reachable], reachable)]
          selected <- utils::head(ranked, 3L)
          ranks <- seq_along(selected)
          nearest_name[i, ranks] <- hospital_names[selected]
          nearest_time[i, ranks] <- duration[selected]
          nearest_distance[i, ranks] <- distance[selected]
          if (length(reachable) < 3L) {
            location_issues <- c(location_issues, paste0(
              "Only ", length(reachable), " hospitals are reachable; unfilled ranks are NA."
            ))
          }
        }
        fixed_time[i, ] <- duration[fixed_destinations]
        fixed_distance[i, ] <- distance[fixed_destinations]
        no_route <- queried[fixed_destinations] & is.na(distance[fixed_destinations])
        if (any(no_route)) {
          location_issues <- c(location_issues, paste0("No route to ", fixed_names[no_route], "."))
        }
        if (length(location_issues)) {
          issue <- paste0("Input row ", unique_rows[i], ": ",
                          paste(unique(location_issues), collapse = " "))
          if (on_error == "stop") stop(issue, call. = FALSE)
          failure_count <- failure_count + 1L
          if (is.null(first_failure)) first_failure <- issue
        }
      }
    }
  }
  if (failure_count && on_error == "warn") {
    warning(failure_count, " unique location(s) had incomplete hospital results. ",
            "First issue: ", first_failure, call. = FALSE)
  }

  column_values <- c(
    unlist(lapply(1:3, function(rank) {
      list(nearest_name[, rank], nearest_time[, rank], nearest_distance[, rank])
    }), recursive = FALSE),
    list(fixed_time[, 1L], fixed_distance[, 1L], fixed_time[, 2L], fixed_distance[, 2L])
  )
  result <- dataset
  for (j in seq_along(output_names)) {
    column <- if (j %in% c(1L, 4L, 7L)) {
      rep(NA_character_, nrow(dataset))
    } else {
      rep(NA_real_, nrow(dataset))
    }
    column[valid_rows] <- column_values[[j]][group]
    if (travel_time_format && output_names[j] %in% time_columns) {
      column <- Moose_second2format(column)
    }
    result[[output_names[j]]] <- column
  }
  result
}

# Stop with an installation hint unless sf and osrm are available.
moose_require_sf_osrm <- function() {
  minimum <- c(sf = "1.0-0", osrm = "4.0.0")
  available <- vapply(names(minimum), function(package) {
    requireNamespace(package, quietly = TRUE) &&
      utils::packageVersion(package) >= minimum[[package]]
  }, logical(1))
  if (!all(available)) {
    needed <- names(minimum)[!available]
    stop("`Moose_nearest_hospitals_2()` needs the ",
         paste0(needed, " (>= ", minimum[needed], ")", collapse = " and "),
         " package", if (length(needed) > 1L) "s",
         ". Install with install.packages(c(\"sf\", \"osrm\")).", call. = FALSE)
  }
  invisible(TRUE)
}

# WGS 84 longitude and latitude of an sf object's POINT geometry. Empty points
# return NA; other geometry types and a missing CRS are errors.
moose_sf_lonlat <- function(x, argument) {
  geometry <- sf::st_geometry(x)
  if (is.na(sf::st_crs(geometry))) {
    stop("`", argument, "` has no coordinate reference system; set one with ",
         "sf::st_set_crs().", call. = FALSE)
  }
  types <- as.character(sf::st_geometry_type(geometry, by_geometry = TRUE))
  if (!all(types == "POINT")) {
    stop("`", argument, "` geometry must contain only POINT features.", call. = FALSE)
  }
  lon <- lat <- rep(NA_real_, length(geometry))
  present <- !sf::st_is_empty(geometry)
  if (any(present)) {
    coordinates <- sf::st_coordinates(sf::st_transform(geometry[present], 4326))
    lon[present] <- coordinates[, "X"]
    lat[present] <- coordinates[, "Y"]
  }
  list(lon = lon, lat = lat)
}

# WGS 84 sf points, the input format used for osrm::osrmTable().
moose_sf_points <- function(lon, lat) {
  sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
}

# Space requests at least `delay` seconds apart, also across calls, so loops
# over Moose_nearest_hospitals_2() respect a server's rate limit.
.moose_osrm_pace <- new.env(parent = emptyenv())

moose_osrm_wait <- function(delay) {
  last <- .moose_osrm_pace$last
  if (delay > 0 && !is.null(last)) {
    remaining <- delay - (as.numeric(Sys.time()) - last)
    if (remaining > 0) Sys.sleep(remaining)
  }
  .moose_osrm_pace$last <- as.numeric(Sys.time())
  invisible(NULL)
}

# Distance units of osrm::osrmTable(): kilometres from osrm 6.0.0, metres before.
moose_osrm_metres_per_unit <- function() {
  if (utils::packageVersion("osrm") >= "6.0.0") 1000 else 1
}

# Route sf origins to sf destinations with osrm::osrmTable(). Returns seconds
# and metres as matrices with one row per origin; NA means no route.
moose_osrm_pkg_table <- function(sources, destinations, server, profile) {
  table <- tryCatch(
    # osrm 6 prints a once-per-session note on units, which are converted below.
    suppressMessages(osrm::osrmTable(
      src = sources, dst = destinations, measure = c("duration", "distance"),
      osrm.server = server, osrm.profile = profile
    )),
    error = function(e) {
      stop(trimws(gsub("\\s+", " ", conditionMessage(e))), call. = FALSE)
    }
  )
  if (!is.list(table)) {
    stop("osrm returned an invalid route table.", call. = FALSE)
  }
  n_sources <- nrow(sources)
  n_destinations <- nrow(destinations)
  read_matrix <- function(x, field) {
    valid_type <- is.numeric(x) || (is.logical(x) && all(is.na(x)))
    if (is.null(x) || !valid_type || length(x) != n_sources * n_destinations ||
        (!is.null(dim(x)) && !identical(as.integer(dim(x)), c(n_sources, n_destinations)))) {
      stop("osrm returned an invalid `", field, "` matrix.", call. = FALSE)
    }
    values <- matrix(as.numeric(x), n_sources, n_destinations)
    if (any(!is.na(values) & (!is.finite(values) | values < 0))) {
      stop("osrm returned an invalid `", field, "` value.", call. = FALSE)
    }
    values
  }
  minutes <- read_matrix(table$durations, "durations")
  distance <- read_matrix(table$distances, "distances")
  if (any(is.na(minutes) != is.na(distance))) {
    stop("osrm returned inconsistent distance and duration availability.", call. = FALSE)
  }
  list(duration = round(minutes * 60, 3),
       distance = round(distance * moose_osrm_metres_per_unit(), 3))
}
