# Expand the precomputed FMC travel grid into a table with one row per grid
# point, for viewing and export. Run from the package root after
# data-raw/create-fmc-travel-grid.R, which also sources this file at its end:
#
#   source("data-raw/create-fmc-travel-points.R")
#
# No network access or routing server is needed.

grid_env <- new.env()
load(file.path("data", "fmc_travel_grid.rda"), envir = grid_env)
travel_grid <- grid_env$fmc_travel_grid

# The Calgary grid covers its region; Alberta points whose whole cell lies
# inside that region are left out so each area has one set of values.
calgary <- travel_grid$grids$calgary
calgary_extent <- list(
  lon_min = calgary$lon0, lon_max = calgary$lon0 + (calgary$nlon - 1) * calgary$dlon,
  lat_min = calgary$lat0, lat_max = calgary$lat0 + (calgary$nlat - 1) * calgary$dlat
)

grid_points <- function(name, exclude = NULL) {
  g <- travel_grid$grids[[name]]
  points <- expand.grid(
    longitude = g$lon0 + (seq_len(g$nlon) - 1) * g$dlon,
    latitude = g$lat0 + (seq_len(g$nlat) - 1) * g$dlat
  ) # longitude varies fastest, matching the nlon x nlat matrices
  minutes <- round(as.vector(g$time_s) / 60, 1)
  km <- round(as.vector(g$distance_m) / 1000, 2)
  keep <- !is.na(minutes) & !is.na(km)
  if (!is.null(exclude)) {
    keep <- keep & !(
      points$longitude - g$dlon / 2 >= exclude$lon_min &
        points$longitude + g$dlon / 2 <= exclude$lon_max &
        points$latitude - g$dlat / 2 >= exclude$lat_min &
        points$latitude + g$dlat / 2 <= exclude$lat_max
    )
  }
  lon <- points$longitude[keep]
  lat <- points$latitude[keep]
  data.frame(
    grid = rep(name, length(lon)),
    longitude = round(lon, 6),
    latitude = round(lat, 6),
    lon_min = round(lon - g$dlon / 2, 6),
    lon_max = round(lon + g$dlon / 2, 6),
    lat_min = round(lat - g$dlat / 2, 6),
    lat_max = round(lat + g$dlat / 2, 6),
    fmc_travel_time_minutes = minutes[keep],
    fmc_travel_distance_km = km[keep],
    stringsAsFactors = FALSE
  )
}

fmc_travel_points <- rbind(
  grid_points("calgary"),
  grid_points("alberta", exclude = calgary_extent)
)
rownames(fmc_travel_points) <- NULL
stopifnot(
  nrow(fmc_travel_points) > 100000L,
  !anyNA(fmc_travel_points),
  identical(unique(fmc_travel_points$grid), c("calgary", "alberta"))
)

dir.create("data", showWarnings = FALSE)
points_file <- file.path("data", "fmc_travel_points.rda")
save(fmc_travel_points, file = points_file, compress = "xz", version = 2)
message(sprintf("Saved %s: %d rows (%d calgary, %d alberta), %.1f MB.",
                points_file, nrow(fmc_travel_points),
                sum(fmc_travel_points$grid == "calgary"),
                sum(fmc_travel_points$grid == "alberta"),
                file.size(points_file) / 1024^2))
