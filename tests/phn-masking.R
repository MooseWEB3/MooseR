library(MooseR)

input <- c(
  "PHN: 123456789",
  "phn 12345-6789",
  "PhN number is 12345 - 6789",
  "123456789 (PHN)",
  "12345-6789 is the phn",
  "PHN 123456789 and phn: 98765-4321",
  "Unrelated number 123456789",
  "Phone 7801234567",
  "SPHN 123456789",
  "PHN 1234567890",
  "PHN\n123456789",
  NA_character_,
  ""
)

expected <- c(
  "PHN: [PHN]",
  "phn [PHN]",
  "PhN number is [PHN]",
  "[PHN] (PHN)",
  "[PHN] is the phn",
  "PHN [PHN] and phn: [PHN]",
  "Unrelated number 123456789",
  "Phone 7801234567",
  "SPHN 123456789",
  "PHN 1234567890",
  "PHN\n123456789",
  NA_character_,
  ""
)

stopifnot(
  identical(Moose_mask_PHN(input), expected),
  identical(
    Moose_mask_PHN_flag(input),
    c(1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
  ),
  identical(Moose_mask_PHN_flag(factor(input[1:2])), c(1L, 1L)),
  identical(
    Moose_mask_PHN_flag("PHN record: 123456789", proximity = 2L),
    0L
  ),
  identical(
    Moose_mask_PHN("PHN=123456789", replacement = "***"),
    "PHN=***"
  ),
  identical(
    Moose_mask_PHN(factor("phn: 12345-6789")),
    "phn: [PHN]"
  ),
  identical(
    Moose_mask_PHN("PHN record: 123456789", proximity = 2L),
    "PHN record: 123456789"
  ),
  inherits(tryCatch(Moose_mask_PHN(123456789), error = identity), "error"),
  inherits(tryCatch(Moose_mask_PHN("PHN 123456789", proximity = -1L), error = identity), "error"),
  inherits(tryCatch(Moose_mask_PHN_flag(123456789), error = identity), "error"),
  inherits(tryCatch(Moose_mask_PHN_flag("PHN 123456789", proximity = -1L), error = identity), "error")
)
