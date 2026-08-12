library(MooseR)
d <- data.frame(
  sex = factor(c("Female", "Female", "Male", "Male", "Male", NA), levels = c("Female", "Male")),
  group = factor(c("Control", "Treatment", "Control", "Treatment", "Treatment", "Control"), levels = c("Control", "Treatment"))
)
row_table <- Moose_cross_table(d, "sex", "group")
stopifnot(
  identical(names(row_table), c("sex", "Control", "Treatment", "Total")),
  identical(row_table$sex, c("Female", "Male", "Missing", "Total")),
  identical(row_table$Control, c("1 (50.0%)", "1 (33.3%)", "1 (100.0%)", "3 (50.0%)")),
  identical(row_table$Total, c("2 (100.0%)", "3 (100.0%)", "1 (100.0%)", "6 (100.0%)"))
)
column_table <- Moose_cross_table(d, "sex", "group", percent = "column", include_missing = FALSE)
stopifnot(
  identical(column_table$Control, c("1 (50.0%)", "1 (50.0%)", "2 (100.0%)")),
  identical(column_table$Total, c("2 (40.0%)", "3 (60.0%)", "5 (100.0%)"))
)
count_table <- Moose_cross_table(d, "sex", "group", percent = "none", include_total = FALSE)
stopifnot(identical(count_table$Control, c("1", "1", "1")))
stopifnot(
  inherits(tryCatch(Moose_cross_table(d, "missing", "group"), error = identity), "error"),
  inherits(tryCatch(Moose_cross_table(d, "sex", "group", percent = "bad"), error = identity), "error")
)
