#' Remove quotes from character/factor columns
#'
#' Strips common quote characters from character and factor columns,
#' leaving other column types unchanged.
#'
#' @param data A data.frame or tibble.
#' @param chars Quote characters to remove, applied literally (not regex).
#'   Default removes straight, curly, and backtick quotes.
#' @return The same data type with quotes removed from relevant columns.
#' @examples
#' df <- data.frame(a = "O'Neil", b = factor("\u201dHello\u201d"), n = 1)
#' BD_quote_rm(df)
#' @export
BD_quote_rm <- function(data,
                        chars = c("'", "\u2019", "\"", "\u201c", "\u201d", "`")) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame")

  strip_chars <- function(x) {
    for (ch in chars) x <- gsub(ch, "", x, fixed = TRUE)
    x
  }

  for (nm in names(data)) {
    if (is.character(data[[nm]])) {
      data[[nm]] <- strip_chars(data[[nm]])
    } else if (is.factor(data[[nm]])) {
      x <- strip_chars(as.character(data[[nm]]))
      data[[nm]] <- factor(x, levels = unique(x))
    }
  }

  data
}
