# Precompute driving time and distance to Foothills Medical Centre on two
# regular grids, for the offline lookup in Moose_travel2FMC().
#
# Run from the package root on the computer with the local Alberta OSRM server
# (see .local-osrm/README.txt; start it with .local-osrm/start.command):
#
#   source("data-raw/create-fmc-travel-grid.R")
#
# Only regularly spaced grid points are routed; no person-level or patient data
# is used. Requires httr2 and jsonlite. Set MOOSER_OSRM_SERVER to use another
# OSRM server with the same Alberta car graph. Takes a few minutes.

server <- sub("/+$", "", Sys.getenv("MOOSER_OSRM_SERVER", "http://127.0.0.1:5001"))
sources_per_request <- 99L # OSRM's default table limit is 100 coordinates.
max_snap_meters <- 5000    # Grid points farther than this from a road get NA.

# Finest grid first: Moose_travel2FMC() uses the first grid covering a point.
grid_specs <- list(
  calgary = list(lon_min = -114.75, lon_max = -113.35, dlon = 0.004,
                 lat_min = 50.55, lat_max = 51.45, dlat = 0.0025),
  alberta = list(lon_min = -120.00, lon_max = -110.00, dlon = 0.015,
                 lat_min = 49.00, lat_max = 60.00, dlat = 0.01)
)

stopifnot(file.exists("DESCRIPTION"), file.exists(file.path("R", "Moose_travel2FMC.R")))
source(file.path("R", "Moose_travel2FMC.R")) # for moose_fmc_lookup()

hospital_data <- new.env()
load(file.path("data", "alberta_hospitals.rda"), envir = hospital_data)
fmc <- hospital_data$alberta_hospitals[
  hospital_data$alberta_hospitals$hospital_name == "Foothills Medical Centre",
  c("hospital_name", "longitude", "latitude")
]
stopifnot(nrow(fmc) == 1L)
rownames(fmc) <- NULL

# Route up to 99 points to FMC in one OSRM table request. `radiuses=unlimited`
# lets every point snap, so one remote point cannot fail the whole request;
# the returned snapping distance then decides which points are kept.
route_to_fmc <- function(lon, lat) {
  n <- length(lon)
  coordinates <- paste0(sprintf("%.6f", c(lon, fmc$longitude)), ",",
                        sprintf("%.6f", c(lat, fmc$latitude)), collapse = ";")
  url <- paste0(
    server, "/table/v1/driving/", coordinates,
    "?sources=", paste(seq_len(n) - 1L, collapse = ";"),
    "&destinations=", n,
    "&annotations=duration,distance",
    "&radiuses=", paste(rep("unlimited", n + 1L), collapse = ";")
  )
  payload <- NULL
  for (attempt in 1:3) {
    payload <- tryCatch({
      response <- httr2::request(url) |>
        httr2::req_timeout(120) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
      httr2::resp_body_json(response, simplifyVector = FALSE)
    }, error = function(e) list(code = "RequestFailed", message = conditionMessage(e)))
    if (identical(payload$code, "Ok")) break
    Sys.sleep(attempt)
  }
  if (!identical(payload$code, "Ok")) {
    stop("OSRM table request failed: ", payload$code, " ",
         if (is.null(payload$message)) "" else payload$message, call. = FALSE)
  }
  cell <- function(field) vapply(payload[[field]], function(row) {
    value <- row[[1L]]
    if (is.null(value)) NA_real_ else as.numeric(value)
  }, numeric(1))
  snap <- vapply(payload$sources, function(waypoint) {
    if (is.null(waypoint$distance)) 0 else as.numeric(waypoint$distance)
  }, numeric(1))
  duration <- cell("durations")
  distance <- cell("distances")
  stopifnot(length(duration) == n, length(distance) == n, length(snap) == n)
  far <- snap > max_snap_meters | is.na(duration) | is.na(distance)
  duration[far] <- NA_real_
  distance[far] <- NA_real_
  list(duration = duration, distance = distance, snap = snap)
}

route_many <- function(lon, lat, label) {
  n <- length(lon)
  duration <- distance <- snap <- rep(NA_real_, n)
  batches <- split(seq_len(n), ceiling(seq_len(n) / sources_per_request))
  started <- Sys.time()
  for (b in seq_along(batches)) {
    rows <- batches[[b]]
    routed <- route_to_fmc(lon[rows], lat[rows])
    duration[rows] <- routed$duration
    distance[rows] <- routed$distance
    snap[rows] <- routed$snap
    if (b %% 500L == 0L || b == length(batches)) {
      elapsed <- as.numeric(difftime(Sys.time(), started, units = "mins"))
      message(sprintf("%s: %d of %d requests (%.1f min)", label, b, length(batches), elapsed))
    }
  }
  list(duration = duration, distance = distance, snap = snap)
}

# Check the server and the Alberta graph before the long run: downtown Calgary
# to FMC must be a short urban route.
check <- route_to_fmc(-114.0719, 51.0447)
if (is.na(check$duration) || check$duration > 30 * 60 || check$distance > 20000) {
  stop("Unexpected test route from downtown Calgary to FMC; check the OSRM server.",
       call. = FALSE)
}
message(sprintf("OSRM check: downtown Calgary to FMC %.1f min, %.1f km",
                check$duration / 60, check$distance / 1000))

grids <- list()
for (name in names(grid_specs)) {
  spec <- grid_specs[[name]]
  nlon <- as.integer(ceiling((spec$lon_max - spec$lon_min) / spec$dlon - 1e-9)) + 1L
  nlat <- as.integer(ceiling((spec$lat_max - spec$lat_min) / spec$dlat - 1e-9)) + 1L
  lon <- spec$lon_min + (seq_len(nlon) - 1L) * spec$dlon
  lat <- spec$lat_min + (seq_len(nlat) - 1L) * spec$dlat
  nodes <- expand.grid(lon = lon, lat = lat) # longitude varies fastest
  message(sprintf("Routing %s grid: %d x %d = %d points", name, nlon, nlat, nrow(nodes)))
  routed <- route_many(nodes$lon, nodes$lat, name)
  grids[[name]] <- list(
    lon0 = spec$lon_min, lat0 = spec$lat_min, dlon = spec$dlon, dlat = spec$dlat,
    nlon = nlon, nlat = nlat,
    time_s = matrix(as.integer(round(routed$duration)), nrow = nlon, ncol = nlat),
    distance_m = matrix(as.integer(round(routed$distance)), nrow = nlon, ncol = nlat)
  )
  message(sprintf("%s grid: %d of %d points have a route", name,
                  sum(!is.na(grids[[name]]$time_s)), nrow(nodes)))
}

# Validate interpolation against exact routes for random locations inside cells
# whose four corners have values, using only locations within 500 m of a road.
set.seed(20261102)
sample_locations <- function(grid, n, outside = NULL) {
  corners <- !is.na(grid$time_s)
  usable <- corners[-grid$nlon, -grid$nlat] & corners[-1L, -grid$nlat] &
    corners[-grid$nlon, -1L] & corners[-1L, -1L]
  cells <- which(usable, arr.ind = TRUE)
  cells <- cells[sample.int(nrow(cells), min(nrow(cells), 3L * n)), , drop = FALSE]
  lon <- grid$lon0 + (cells[, 1L] - 1 + stats::runif(nrow(cells))) * grid$dlon
  lat <- grid$lat0 + (cells[, 2L] - 1 + stats::runif(nrow(cells))) * grid$dlat
  if (!is.null(outside)) {
    keep <- lon < outside$lon_min | lon > outside$lon_max |
      lat < outside$lat_min | lat > outside$lat_max
    lon <- lon[keep]
    lat <- lat[keep]
  }
  keep <- utils::head(seq_along(lon), n)
  list(lon = lon[keep], lat = lat[keep])
}
validate_region <- function(region, locations) {
  exact <- route_many(locations$lon, locations$lat, paste(region, "validation"))
  estimate <- moose_fmc_lookup(locations$lon, locations$lat, list(grids = grids))
  keep <- !is.na(exact$duration) & !is.na(estimate$duration) & exact$snap <= 500
  minutes <- abs(estimate$duration[keep] - exact$duration[keep]) / 60
  km <- abs(estimate$distance[keep] - exact$distance[keep]) / 1000
  q <- function(x, p) round(unname(stats::quantile(x, p)), 2)
  data.frame(
    region = region, locations = sum(keep),
    median_error_minutes = q(minutes, 0.5), p90_error_minutes = q(minutes, 0.9),
    p95_error_minutes = q(minutes, 0.95), median_error_km = q(km, 0.5),
    p90_error_km = q(km, 0.9), p95_error_km = q(km, 0.95)
  )
}
validation <- rbind(
  validate_region("calgary", sample_locations(grids$calgary, 2000L)),
  validate_region("rest_of_alberta",
                  sample_locations(grids$alberta, 2000L, outside = grid_specs$calgary))
)

manifest_file <- file.path(".local-osrm", "install-manifest.json")
osrm_version <- if (file.exists(manifest_file)) {
  jsonlite::read_json(manifest_file)$version
} else {
  NA_character_
}
fmc_travel_grid <- list(
  destination = fmc,
  grids = grids,
  max_snap_meters = max_snap_meters,
  routing = list(
    engine = "OSRM", version = osrm_version, profile = "car",
    graph = "OpenStreetMap Alberta extract", traffic = "none (free-flow)",
    built_on = Sys.Date()
  ),
  validation = validation
)

dir.create("data", showWarnings = FALSE)
output <- file.path("data", "fmc_travel_grid.rda")
save(fmc_travel_grid, file = output, compress = "xz", version = 2)
print(validation)
message(sprintf("Saved %s (%.1f MB).", output, file.size(output) / 1024^2))
