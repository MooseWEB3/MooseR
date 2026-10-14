library(MooseR)

# Concrete identifiers and each common typo operation, including reversal.
stopifnot(
  identical(Moose_similarscore("E123456", "e1223456"), 0.875),
  identical(Moose_similarscore("E123456", "e1223456", ignore_case = FALSE), 0.75),
  identical(Moose_similarscore(c("ABCD", "ABCD", "ABCD", "ABCD"),
                             c("ABD", "ABXCD", "ABXD", "ACBD")),
            c(0.75, 0.8, 0.75, 0.75)),
  identical(Moose_similarscore("abc", "XYZ"), 0),
  identical(Moose_similarscore("Abc", "aBC"), 1),
  identical(Moose_similarscore("ABC", "abc", ignore_case = FALSE), 0),
  # OSA restriction: overlapping edits cannot turn CA into ABC in two steps.
  identical(Moose_similarscore("CA", "ABC"), 0),
  identical(Moose_similarscore("", ""), 1),
  identical(Moose_similarscore(c("", "abc"), c("abc", "")), c(0, 0)),
  identical(Moose_similarscore("AB ", "AB"), 1 - 1 / 3),
  identical(Moose_similarscore("AB-1", "AB1"), 0.75)
)

# Numeric conversion, factor labels, missing values and Unicode characters.
stopifnot(
  identical(Moose_similarscore(c(123456, 123456), c("123456", "123457")),
            c(1, 1 - 1 / 6)),
  identical(Moose_similarscore(c(-12.5, 1e20, Inf),
                             as.character(c(-12.5, 1e20, Inf))), rep(1, 3)),
  identical(Moose_similarscore(123, "00123"), 0.6),
  identical(Moose_similarscore(factor(c("E123456", "ABC")),
                             c("e1223456", "abc")), c(0.875, 1)),
  identical(Moose_similarscore(c(NA_real_, NaN, 123), c(NA, "NaN", "123")),
            c(NA_real_, NA_real_, 1)),
  identical(Moose_similarscore(NA, NA), NA_real_),
  identical(Moose_similarscore("NA", "NA"), 1),
  identical(Moose_similarscore("\u533b\u9662A", "\u533b\u9662B"), 1 - 1 / 3),
  identical(Moose_similarscore("\u00e9A", "\u00e9B"), 0.5)
)

# Pairwise columns and scalar broadcasting must retain row order and names.
records <- data.frame(original = c("E123456", "ABC", NA),
                      entered = c("e1223456", "abc", "XYZ"))
records$similarity <- Moose_similarscore(records$original, records$entered)
stopifnot(
  identical(records$similarity, c(0.875, 1, NA_real_)),
  identical(Moose_similarscore(c(exact = "ABC", typo = "ABD"), "abc"),
            c(exact = 1, typo = 1 - 1 / 3)),
  identical(Moose_similarscore("abc", c(exact = "ABC", typo = "ABD")),
            c(exact = 1, typo = 1 - 1 / 3)),
  identical(Moose_similarscore(c(x = "ABC"), c(y = "abc")), c(x = 1)),
  identical(Moose_similarscore(c("ABC", "DEF"), c(ref = "abc")), c(1, 0)),
  identical(Moose_similarscore(c(ref = "abc"), c("ABC", "DEF")), c(1, 0)),
  identical(Moose_similarscore(numeric(), character()), numeric()),
  identical(Moose_similarscore(character(), "ABC"), numeric()),
  identical(Moose_similarscore("ABC", character()), numeric())
)

# All-pairs checks on short strings verify bounds, symmetry and identity.
strings <- c("", "a", "b", "ab", "ba", "aa", "aba", "bab", "abba", "baba")
pairs <- expand.grid(x = strings, y = strings, stringsAsFactors = FALSE)
scores <- Moose_similarscore(pairs$x, pairs$y)
stopifnot(
  all(is.finite(scores) & scores >= 0 & scores <= 1),
  identical(scores, Moose_similarscore(pairs$y, pairs$x)),
  all(scores[pairs$x == pairs$y] == 1)
)

must_error <- function(expression, pattern) {
  error <- tryCatch(force(expression), error = identity)
  stopifnot(inherits(error, "error"),
            grepl(pattern, conditionMessage(error), fixed = TRUE))
}
must_error(Moose_similarscore(1:2, 1:3), "equal lengths")
must_error(Moose_similarscore(character(), 1:2), "equal lengths")
for (invalid in list(TRUE, 1 + 1i, list("ABC"), matrix("ABC"),
                     data.frame(x = "ABC"), as.Date("2026-10-14"))) {
  must_error(Moose_similarscore(invalid, "ABC"), "`x`")
  must_error(Moose_similarscore("ABC", invalid), "`y`")
}
for (invalid in list(NA, 1, "TRUE", c(TRUE, FALSE), logical(), matrix(TRUE))) {
  must_error(Moose_similarscore("ABC", "abc", ignore_case = invalid), "ignore_case")
}
