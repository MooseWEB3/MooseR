library(MooseR)

must_error <- function(expr, pattern) {
  error <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error), fixed = TRUE))
}

# Interpolation on a synthetic grid. Values rise by 60 s per step in either
# direction; one corner of the fine grid has no road, and a coarse grid
# covers what the fine grid cannot.
lookup <- MooseR:::moose_fmc_lookup
synthetic <- list(grids = list(
  fine = list(lon0 = 0, lat0 = 0, dlon = 1, dlat = 1, nlon = 3L, nlat = 3L,
              time_s = matrix(c(0L, 60L, 120L, 60L, 120L, 180L, 120L, 180L, NA), 3L, 3L),
              distance_m = matrix(c(0L, 600L, 1200L, 600L, 1200L, 1800L, 1200L, 1800L, NA),
                                  3L, 3L)),
  coarse = list(lon0 = -10, lat0 = -10, dlon = 5, dlat = 5, nlon = 5L, nlat = 5L,
                time_s = matrix(600L, 5L, 5L), distance_m = matrix(6000L, 5L, 5L))
))
lon <- c(0.5, 1.25, 1.5, 2, 2, 5, 50, NA, 0, 0, 2)
lat <- c(0.5, 0.5, 1.5, 0, 2, 5, 50, 1, 95, 0, 1)
estimate <- lookup(lon, lat, synthetic)
stopifnot(
  isTRUE(all.equal(estimate$duration, c(60, 105, 160, 120, 600, 600, NA, NA, NA, 0, 180))),
  isTRUE(all.equal(estimate$distance,
                   c(600, 1050, 1600, 1200, 6000, 6000, NA, NA, NA, 0, 1800)))
)
empty_estimate <- lookup(numeric(), numeric(), synthetic)
stopifnot(length(empty_estimate$duration) == 0L, length(empty_estimate$distance) == 0L)

# The bundled grid is complete and consistent.
grid <- MooseR:::moose_fmc_travel_grid()
stopifnot(
  identical(names(grid$grids), c("calgary", "alberta")),
  identical(grid$destination$hospital_name, "Foothills Medical Centre"),
  all(vapply(grid$grids, function(g) {
    identical(dim(g$time_s), c(g$nlon, g$nlat)) &&
      identical(dim(g$distance_m), c(g$nlon, g$nlat)) &&
      identical(is.na(g$time_s), is.na(g$distance_m)) &&
      any(!is.na(g$time_s))
  }, logical(1))),
  is.data.frame(grid$validation),
  identical(grid$validation$region, c("calgary", "rest_of_alberta"))
)

# Plausible values at FMC, downtown Calgary and downtown Edmonton.
places <- data.frame(
  id = 1:6,
  lon = c(-114.13395, -114.0719, -113.4938, NA, -100, -114.0719),
  lat = c(51.064434, 51.0447, 53.5461, 51, 51, 51.0447)
)
outputs <- c("fmc_travel_time_minutes", "fmc_travel_distance_km")
result <- Moose_travel2FMC(places, longitude = "lon", latitude = "lat")
stopifnot(
  identical(names(result), c(names(places), outputs)),
  identical(result[names(places)], places),
  all(vapply(result[outputs], is.double, logical(1))),
  result$fmc_travel_time_minutes[1] < 8, result$fmc_travel_distance_km[1] < 5,
  result$fmc_travel_time_minutes[2] > 3, result$fmc_travel_time_minutes[2] < 30,
  result$fmc_travel_distance_km[2] > 2, result$fmc_travel_distance_km[2] < 20,
  result$fmc_travel_time_minutes[3] > 150, result$fmc_travel_time_minutes[3] < 240,
  result$fmc_travel_distance_km[3] > 270, result$fmc_travel_distance_km[3] < 340,
  all(is.na(result[4:5, outputs])),
  identical(unname(unlist(result[2, outputs])), unname(unlist(result[6, outputs])))
)

# Numeric strings and factors, zero rows, custom names and overwriting.
text_input <- data.frame(lon = factor("-114.0719"), lat = "51.0447")
stopifnot(identical(
  Moose_travel2FMC(text_input, "lon", "lat")$fmc_travel_time_minutes,
  result$fmc_travel_time_minutes[2]
))
empty <- Moose_travel2FMC(places[FALSE, ], "lon", "lat")
stopifnot(nrow(empty) == 0L, identical(names(empty), names(result)))
custom <- Moose_travel2FMC(places, "lon", "lat", time_column = "minutes",
                           distance_column = "km")
stopifnot(identical(unname(custom$minutes), unname(result$fmc_travel_time_minutes)),
          identical(unname(custom$km), unname(result$fmc_travel_distance_km)))
must_error(Moose_travel2FMC(result, "lon", "lat"), "Output columns already exist")
stopifnot(identical(Moose_travel2FMC(result, "lon", "lat", overwrite = TRUE), result))
must_error(Moose_travel2FMC(1:3), "data frame")
must_error(Moose_travel2FMC(places), "Unknown coordinate columns")
must_error(Moose_travel2FMC(places, "lon", "lat", time_column = "x", distance_column = "x"),
           "must be different")
must_error(Moose_travel2FMC(places, "lon", "lat", overwrite = NA), "overwrite")
