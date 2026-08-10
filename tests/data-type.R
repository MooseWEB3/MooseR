library(MooseR)

data_types <- data.frame(
  continuous_double = c(1.5, 2.5),
  categorical_character = c("A", "B"),
  date_value = as.Date(c("2026-01-01", "2026-01-02")),
  categorical_factor = factor(c("yes", "no")),
  continuous_integer = c(1L, 2L),
  categorical_logical = c(TRUE, FALSE),
  datetime_value = as.POSIXct(
    c("2026-01-01 10:00:00", "2026-01-02 11:00:00"),
    tz = "UTC"
  ),
  stringsAsFactors = FALSE
)
data_types$list_value <- list(1:2, 3:4)

selected <- c(
  "continuous_double",
  "categorical_character",
  "date_value",
  "categorical_factor",
  "continuous_integer",
  "categorical_logical",
  "datetime_value",
  "list_value"
)

stopifnot(
  identical(
    Moose_data_type(data_types, selected),
    c(
      continuous_double = "continuous",
      categorical_character = "categorical",
      date_value = "date",
      categorical_factor = "categorical",
      continuous_integer = "continuous",
      categorical_logical = "categorical",
      datetime_value = "date",
      list_value = "other"
    )
  ),
  identical(Moose_data_type(data_types, character()), setNames(character(), character())),
  inherits(tryCatch(Moose_data_type(1:3, "x"), error = identity), "error"),
  inherits(
    tryCatch(Moose_data_type(data_types, "missing"), error = identity),
    "error"
  ),
  inherits(tryCatch(Moose_data_type(data_types, NA_character_), error = identity), "error")
)
