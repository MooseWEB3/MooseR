library(MooseR)

values <- c("1023056", "98765", NA_character_, "", "  ")
cat_value <- paste0(intToUtf8(0x732B), "AB")
accent_value <- paste0(intToUtf8(c(0x65, 0x301)), "x")
family_value <- paste0(
  intToUtf8(c(0x1F468, 0x200D, 0x1F469, 0x200D, 0x1F467)),
  "X"
)

stopifnot(
  identical(
    Moose_mask(values),
    c("*******", "*****", NA_character_, "", "**")
  ),
  identical(
    Moose_mask(values, side = "front", n = 3),
    c("***3056", "***65", NA_character_, "", "**")
  ),
  identical(
    Moose_mask(values, side = "back", n = 3),
    c("1023***", "98***", NA_character_, "", "**")
  ),
  identical(Moose_mask("abc", n = 0), "abc"),
  identical(Moose_mask("abc", side = "back", n = 20), "***"),
  identical(Moose_mask(character()), character()),
  identical(Moose_mask(factor(c("AB", "CD")), n = 1), c("*B", "*D")),
  identical(Moose_mask(c(1023056, 42), n = 3), c("***3056", "**")),
  identical(Moose_mask(c(TRUE, FALSE)), c("****", "*****")),
  identical(Moose_mask(as.Date("2026-08-27")), "**********"),
  identical(Moose_mask(c(cat_value, accent_value), n = 1), c("*AB", "*x")),
  identical(
    Moose_mask(cat_value, side = "back", n = 2),
    paste0(intToUtf8(0x732B), "**")
  ),
  identical(Moose_mask(family_value, n = 1), "*X")
)

named_input <- structure(
  c("alpha", "beta", NA_character_, ""),
  names = c("duplicate", "duplicate", "", NA_character_)
)
named_output <- Moose_mask(named_input, side = "back", n = 2)

stopifnot(
  is.character(named_output),
  identical(length(named_output), length(named_input)),
  identical(names(named_output), names(named_input)),
  identical(unname(named_output), c("alp**", "be**", NA_character_, "")),
  identical(names(Moose_mask(named_input)), names(named_input)),
  identical(formals(Moose_mask)$side, "front"),
  is.null(formals(Moose_mask)$n)
)

invalid_calls <- list(
  function() Moose_mask(NULL),
  function() Moose_mask(list("abc")),
  function() Moose_mask(data.frame(value = "abc")),
  function() Moose_mask(matrix("abc")),
  function() Moose_mask("abc", side = "middle"),
  function() Moose_mask("abc", side = NA_character_),
  function() Moose_mask("abc", side = c("front", "back")),
  function() Moose_mask("abc", n = -1),
  function() Moose_mask("abc", n = 1.5),
  function() Moose_mask("abc", n = Inf),
  function() Moose_mask("abc", n = NA_real_),
  function() Moose_mask("abc", n = TRUE),
  function() Moose_mask("abc", n = c(1, 2)),
  function() Moose_mask("abc", n = "1")
)

stopifnot(all(vapply(
  invalid_calls,
  function(call) inherits(tryCatch(call(), error = identity), "error"),
  logical(1)
)))

sensitive_error <- tryCatch(
  Moose_mask("do-not-repeat-this-value", n = -1),
  error = identity
)
stopifnot(
  inherits(sensitive_error, "error"),
  !grepl("do-not-repeat-this-value", conditionMessage(sensitive_error), fixed = TRUE)
)
