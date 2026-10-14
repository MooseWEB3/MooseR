#' Compare strings with a similarity score from zero to one
#'
#' Compares pairs of strings using normalized optimal string alignment (OSA,
#' or restricted Damerau-Levenshtein) distance. Numeric input is converted to
#' character first, and letter case is ignored by default.
#'
#' @param x,y Character or numeric vectors, or factors (compared by their
#'   labels). Inputs must have equal lengths, or one must have length one to
#'   compare it with every element of the other. A logical vector containing
#'   only `NA` is also accepted. Lists, matrices and other classed objects are
#'   not supported.
#' @param ignore_case A single non-missing logical value. If `TRUE` (the
#'   default), compare strings after applying [base::tolower()].
#' @return A numeric vector of pairwise scores between 0 and 1. Names come
#'   from an input of the result's length, preferring `x` when both are named.
#'   A missing value in either input gives `NA_real_`. Two empty strings score
#'   1; an empty string compared with a non-empty string scores 0. Comparing
#'   zero-length inputs, or a zero-length input with a scalar, returns
#'   `numeric(0)`.
#' @details
#' The score is `1 - distance / max(length_x, length_y)`, using character
#' counts after case conversion. Each insertion, deletion, substitution or
#' adjacent transposition costs one edit. OSA does not allow a substring to
#' be edited more than once. A score of 1 means equality after case conversion;
#' a score of 0 means the edit distance equals the longer string's length.
#' A score of 0.99 represents 99 percent similarity under this rule, not a
#' probability that two values identify the same person or record.
#'
#' Whitespace and punctuation are retained. Comparisons use Unicode code
#' points, not bytes; Unicode normalization is not performed. Case conversion
#' follows the current R locale. Numeric values use [base::as.character()]
#' formatting, which may include scientific notation. Supply identifiers as
#' character strings to retain leading zeros and digits beyond R's numeric
#' precision. Numeric `NaN` is treated as missing.
#' @references
#' van der Loo, M. P. J. (2014). The stringdist Package for Approximate String
#' Matching. The R Journal, 6(1), 111-122.
#' \url{https://journal.r-project.org/articles/RJ-2014-011/}
#' @examples
#' Moose_similarscore("E123456", "e1223456") # 0.875
#' Moose_similarscore(123456, "123456")     # 1
#' Moose_similarscore("AB12", "AB21")      # 0.75: adjacent transposition
#' Moose_similarscore("ABC", "abc", ignore_case = FALSE) # 0
#'
#' records <- data.frame(
#'   original = c("E123456", "ABC", NA),
#'   entered = c("e1223456", "abc", "XYZ")
#' )
#' records$similarity <- Moose_similarscore(records$original, records$entered)
#' @export
Moose_similarscore <- function(x, y, ignore_case = TRUE) {
  if (!is.logical(ignore_case) || length(ignore_case) != 1L ||
      is.na(ignore_case) || !is.null(dim(ignore_case))) {
    stop("`ignore_case` must be TRUE or FALSE.", call. = FALSE)
  }

  as_strings <- function(value, argument) {
    plain_vector <- !is.object(value) &&
      (is.character(value) || (is.numeric(value) && !is.complex(value)) ||
       (is.logical(value) && all(is.na(value))))
    if (!(plain_vector || is.factor(value)) || !is.null(dim(value))) {
      stop(sprintf("`%s` must be a character or numeric vector, or a factor.",
                   argument), call. = FALSE)
    }
    missing <- is.na(value)
    result <- enc2utf8(as.character(value))
    result[missing] <- NA_character_
    if (ignore_case) result <- tolower(result)
    result
  }

  x_names <- names(x)
  y_names <- names(y)
  x <- as_strings(x, "x")
  y <- as_strings(y, "y")
  nx <- length(x)
  ny <- length(y)
  if (nx != ny && nx != 1L && ny != 1L) {
    stop("`x` and `y` must have equal lengths, or one must have length one.",
         call. = FALSE)
  }
  if (nx == 0L || ny == 0L) return(numeric(0))

  size <- max(nx, ny)
  result_names <- if (nx == size && !is.null(x_names)) {
    x_names
  } else if (ny == size) {
    y_names
  } else {
    NULL
  }
  if (nx == 1L) x <- rep(x, size)
  if (ny == 1L) y <- rep(y, size)

  result <- rep(NA_real_, size)
  for (i in which(!is.na(x) & !is.na(y))) {
    result[i] <- moose_osa_similarity(x[i], y[i])
  }
  names(result) <- result_names
  result
}

# Use three rolling rows so memory grows with the shorter string, rather
# than allocating a full distance matrix (or all pairs of input vectors).
moose_osa_similarity <- function(x, y) {
  if (x == y) return(1)
  a <- utf8ToInt(x)
  b <- utf8ToInt(y)
  if (length(a) < length(b)) {
    swap <- a
    a <- b
    b <- swap
  }
  na <- length(a)
  nb <- length(b)
  if (nb == 0L) return(0)

  previous <- seq.int(0, nb)
  previous_two <- NULL
  for (i in seq_len(na)) {
    current <- c(i, numeric(nb))
    for (j in seq_len(nb)) {
      current[j + 1L] <- min(
        previous[j + 1L] + 1,
        current[j] + 1,
        previous[j] + (a[i] != b[j])
      )
      if (i > 1L && j > 1L && a[i] == b[j - 1L] && a[i - 1L] == b[j]) {
        current[j + 1L] <- min(current[j + 1L], previous_two[j - 1L] + 1)
      }
    }
    previous_two <- previous
    previous <- current
  }
  1 - previous[nb + 1L] / na
}
