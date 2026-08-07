library(MooseR)

expect_moose_error <- function(expr, pattern) {
  error <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )

  stopifnot(
    inherits(error, "error"),
    grepl(pattern, conditionMessage(error), fixed = TRUE)
  )
}

categorical <- data.frame(
  group = c("Zulu", "Zulu", "Alpha", "Bravo"),
  stringsAsFactors = FALSE
)

top_result <- Moose_1_cat(
  categorical,
  "group",
  sort_levels = "alpha",
  top_n = 1
)

stopifnot(
  identical(names(top_result), c("group", "Zulu", "Other", "Total")),
  identical(top_result[["Zulu"]], "2 (50.0%)"),
  identical(top_result[["Other"]], "2 (50.0%)"),
  identical(top_result[["Total"]], "4 (100.0%)")
)

collision_result <- Moose_1_cat(
  data.frame(group = c("Other", "Other", "Alpha", "Bravo")),
  "group",
  top_n = 1
)

stopifnot(
  identical(
    names(collision_result),
    c("group", "Other", "Other_1", "Total")
  ),
  !anyDuplicated(names(collision_result))
)

empty_cat <- Moose_1_cat(data.frame(group = character()), "group")
stopifnot(
  identical(names(empty_cat), c("group", "Total")),
  identical(empty_cat[["Total"]], "0 (0.0%)")
)

expect_moose_error(
  Moose_1_cat(categorical, "group", top_n = 1.5),
  "`top_n` must be NULL or one positive integer."
)
expect_moose_error(
  Moose_1_cat(categorical, "group", include_missing = NA),
  "`include_missing` must be TRUE or FALSE."
)

continuous <- data.frame(value = c(1, 2, Inf, NA_real_))
continuous_result <- Moose_1_cont(
  continuous,
  "value",
  probs = c(0.20, 0.80)
)

stopifnot(
  identical(names(continuous_result)[5:6], c("P20", "P80")),
  identical(continuous_result[["P20"]], "1.20"),
  identical(continuous_result[["P80"]], "1.80"),
  continuous_result[["Missing"]] == 1L,
  continuous_result[["N"]] == 4L,
  continuous_result[["NonMissing"]] == 3L,
  continuous_result[["NonFinite"]] == 1L,
  continuous_result[["Analyzed"]] == 2L
)

empty_cont <- Moose_1_cont(data.frame(value = numeric()), "value")
stopifnot(
  empty_cont[["N"]] == 0L,
  empty_cont[["Analyzed"]] == 0L,
  is.na(empty_cont[["MissingPct"]])
)

expect_moose_error(
  Moose_1_cont(continuous, "value", probs = c(0.5, 0.5)),
  "`probs` must not contain duplicates."
)
expect_moose_error(
  Moose_1_cont(continuous, "value", probs = 1.1),
  "`probs` must contain finite, non-missing numbers between 0 and 1."
)
expect_moose_error(
  Moose_1_cont(continuous, "value", finite_only = 1),
  "`finite_only` must be TRUE or FALSE."
)

# Legacy function names remain behaviorally identical.
stopifnot(
  identical(
    Moose_1_cat(categorical, "group"),
    BD_1_cat(categorical, "group")
  ),
  identical(
    Moose_1_cont(continuous, "value"),
    BD_1_cont(continuous, "value")
  )
)
