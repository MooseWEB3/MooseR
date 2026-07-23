#' Read a CSV file and clean its column names
#'
#' Reads a CSV file with base R and standardizes its column names
#' by replacing spaces and slashes with underscores and removing parentheses,
#' commas, and hyphens.
#'
#' @param path Directory containing the CSV file.
#' @param file_name Name of the CSV file.
#'
#' @return A data frame containing the imported data with cleaned column names.
#'
#' @examples
#' csv_file <- tempfile(fileext = ".csv")
#' writeLines("First Name,Value/Score\nAlice,10", csv_file)
#' Moose_read_csv(dirname(csv_file), basename(csv_file))
#' unlink(csv_file)
#'
#' @export
Moose_read_csv <- function(path, file_name) {
  data_imported <- utils::read.csv(
    file = file.path(path, file_name),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  colnames(data_imported) <- clean_moose_csv_names(colnames(data_imported))

  data_imported
}

clean_moose_csv_names <- function(x) {
  x <- gsub(" ", "_", x, fixed = TRUE)
  x <- gsub("/", "_", x, fixed = TRUE)
  x <- gsub("[(),]", "", x)
  x <- gsub("-", "", x, fixed = TRUE)
  x
}
