#' Estimate driving time and distance to Foothills Medical Centre offline
#'
#' Adds the estimated driving time (minutes) and road distance (kilometres)
#' from each location to Foothills Medical Centre in Calgary, read from the
#' precomputed [fmc_travel_grid]. No network access is needed: coordinates
#' never leave the computer.
#'
#' The grid holds OSRM car routes, on the Alberta OpenStreetMap road graph and
#' without live traffic, from regularly spaced points to Foothills Medical
#' Centre: about every 280 m in the Calgary region (longitude -114.75 to
#' -113.35, latitude 50.55 to 51.45) and about every 1 km elsewhere in Alberta
#' (longitude -120 to -110, latitude 49 to 60). Each location is estimated by
#' bilinear interpolation between the four surrounding grid points of the
#' finest grid that covers it.
#'
#' Grid points with no road within `fmc_travel_grid$max_snap_meters` have no
#' value and are left out of the interpolation. A location whose four
#' surrounding points all lack values, or that lies outside both grids, returns
#' `NA`, as do missing, non-numeric and out-of-range coordinates. Locations
#' just outside Alberta can still receive values from nearby Alberta roads.
#'
#' Results are estimates. They differ most from routing the exact coordinate
#' where neighbouring points reach different roads, for example across a river
#' or a highway without a nearby crossing; `fmc_travel_grid$validation` reports
#' the measured error against exact routes. Times are free-flow estimates and
#' exclude traffic, weather, turns into parking and walking. Use
#' [Moose_nearest_hospitals()] or [Moose_travel()] to route exact coordinates
#' through a routing server.
#'
#' @param dataset A data frame or tibble. Original rows and columns are retained.
#' @param longitude,latitude Names of columns containing coordinates in decimal
#'   degrees. Numeric strings and factors are accepted.
#' @param time_column Name of the output column for driving time in minutes.
#' @param distance_column Name of the output column for road distance in
#'   kilometres.
#' @param overwrite Logical. Replace existing output columns if `TRUE`.
#'
#' @return `dataset` with two numeric columns added: estimated driving time in
#'   minutes, rounded to one decimal place, and road distance in kilometres,
#'   rounded to two decimal places.
#' @seealso [fmc_travel_grid], [Moose_nearest_hospitals()], [Moose_travel()]
#' @examples
#' locations <- data.frame(longitude = c(-114.0719, -113.4938),
#'                         latitude = c(51.0447, 53.5461))
#' Moose_travel2FMC(locations)
#'
#' # Custom column names:
#' events <- data.frame(Event_Longitude = -114.0719, Event_Latitude = 51.0447)
#' Moose_travel2FMC(events, longitude = "Event_Longitude", latitude = "Event_Latitude")
#' @export
Moose_travel2FMC <- function(dataset,
                             longitude = "longitude",
                             latitude = "latitude",
                             time_column = "fmc_travel_time_minutes",
                             distance_column = "fmc_travel_distance_km",
                             overwrite = FALSE) {
  if (!is.data.frame(dataset) || anyDuplicated(names(dataset))) {
    stop("`dataset` must be a data frame or tibble with unique column names.", call. = FALSE)
  }
  longitude <- moose_check_text(longitude, "longitude")
  latitude <- moose_check_text(latitude, "latitude")
  time_column <- moose_check_text(time_column, "time_column")
  distance_column <- moose_check_text(distance_column, "distance_column")
  if (identical(time_column, distance_column)) {
    stop("`time_column` and `distance_column` must be different.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  missing_columns <- setdiff(c(longitude, latitude), names(dataset))
  if (length(missing_columns)) {
    stop("Unknown coordinate columns: ", paste(missing_columns, collapse = ", "), ".",
         call. = FALSE)
  }
  conflicts <- intersect(c(time_column, distance_column), names(dataset))
  if (length(conflicts) && !overwrite) {
    stop("Output columns already exist: ", paste(conflicts, collapse = ", "),
         ". Use `overwrite = TRUE` to replace them.", call. = FALSE)
  }

  estimate <- moose_fmc_lookup(
    moose_as_coordinate(dataset[[longitude]]),
    moose_as_coordinate(dataset[[latitude]]),
    moose_fmc_travel_grid()
  )
  dataset[[time_column]] <- round(estimate$duration / 60, 1)
  dataset[[distance_column]] <- round(estimate$distance / 1000, 2)
  dataset
}

# The bundled grid is several megabytes; load it once per session.
.moose_fmc_cache <- new.env(parent = emptyenv())

moose_fmc_travel_grid <- function() {
  if (is.null(.moose_fmc_cache$grid)) {
    bundled <- new.env(parent = emptyenv())
    suppressWarnings(utils::data("fmc_travel_grid", package = "MooseR", envir = bundled))
    if (!exists("fmc_travel_grid", envir = bundled, inherits = FALSE)) {
      stop("This MooseR installation does not include `fmc_travel_grid`. Build it ",
           "with data-raw/create-fmc-travel-grid.R, then reinstall MooseR.", call. = FALSE)
    }
    .moose_fmc_cache$grid <- bundled$fmc_travel_grid
  }
  .moose_fmc_cache$grid
}

# Bilinear interpolation of route seconds and metres. Grids are tried in order
# (finest first); a location falls through to the next grid when it lies
# outside a grid or all four surrounding points lack values. Each grid stores
# `nlon` x `nlat` matrices, so point (i, j), counted from 0, has linear index
# i + j * nlon + 1.
moose_fmc_lookup <- function(lon, lat, grid_data) {
  duration <- distance <- rep(NA_real_, length(lon))
  pending <- is.finite(lon) & is.finite(lat) & abs(lon) <= 180 & abs(lat) <= 90
  for (grid in grid_data$grids) {
    if (!any(pending)) break
    x <- (lon - grid$lon0) / grid$dlon
    y <- (lat - grid$lat0) / grid$dlat
    # A small tolerance keeps points on a grid's outer edge inside that grid
    # despite floating-point error (for example latitude 51.45 in Calgary).
    tolerance <- 1e-9
    rows <- which(pending & x >= -tolerance & y >= -tolerance &
                    x <= grid$nlon - 1 + tolerance & y <= grid$nlat - 1 + tolerance)
    if (!length(rows)) next
    i0 <- pmin(pmax(floor(x[rows]), 0), grid$nlon - 2)
    j0 <- pmin(pmax(floor(y[rows]), 0), grid$nlat - 2)
    fx <- pmin(pmax(x[rows] - i0, 0), 1)
    fy <- pmin(pmax(y[rows] - j0, 0), 1)
    weights <- cbind((1 - fx) * (1 - fy), fx * (1 - fy), (1 - fx) * fy, fx * fy)
    cells <- cbind(i0, i0 + 1, i0, i0 + 1) + cbind(j0, j0, j0 + 1, j0 + 1) * grid$nlon + 1
    seconds <- matrix(as.numeric(grid$time_s[c(cells)]), ncol = 4L)
    metres <- matrix(as.numeric(grid$distance_m[c(cells)]), ncol = 4L)
    weights[is.na(seconds) | is.na(metres)] <- 0
    total <- rowSums(weights)
    valued <- total > 1e-9
    seconds[is.na(seconds)] <- 0
    metres[is.na(metres)] <- 0
    duration[rows[valued]] <- (rowSums(weights * seconds) / total)[valued]
    distance[rows[valued]] <- (rowSums(weights * metres) / total)[valued]
    pending[rows[valued]] <- FALSE
  }
  list(duration = duration, distance = distance)
}
