#' Flag text values containing a detected personal name
#'
#' Uses the same spaCy or pure R regex engine as
#' [Moose_detect_person_names()] to inspect a character or factor vector.
#' The returned integer vector can be assigned directly inside
#' `dplyr::mutate()`. `1L` means at least one personal name was detected in the
#' corresponding value, and `0L` means no name was detected. Missing and blank
#' values receive `0L`.
#'
#' @param text A character or factor vector, usually a column from a data frame.
#' @param batch_size Number of documents processed per spaCy batch.
#' @param engine Character. One of `"auto"`, `"spacy"`, or `"regex"`.
#'
#' @return An integer vector with the same length as `text`, containing `1L`
#'   when a personal name is detected and `0L` otherwise.
#'
#' @examples
#' comments <- c(
#'   "John Smith reviewed the file.",
#'   "nothing to report",
#'   NA
#' )
#' Moose_name_flag(comments, engine = "regex")
#'
#' @export
Moose_name_flag <- function(text,
                            batch_size = 100L,
                            engine = c("auto", "spacy", "regex")) {
  engine <- match.arg(engine)

  if (is.factor(text)) {
    text <- as.character(text)
  }

  if (!is.character(text)) {
    stop("`text` must be a character or factor vector.", call. = FALSE)
  }

  detected <- detect_person_names(
    text,
    batch_size = batch_size,
    engine = engine
  )

  flag <- integer(length(text))

  if (nrow(detected)) {
    detected_rows <- unique(detected$row_id)
    detected_rows <- detected_rows[
      !is.na(detected_rows) &
        detected_rows >= 1L &
        detected_rows <= length(text)
    ]
    flag[detected_rows] <- 1L
  }

  flag
}
