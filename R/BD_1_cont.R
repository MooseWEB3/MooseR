#' One-line summary for a continuous variable
#'
#' Produces a single display-ready data frame row summarizing a numeric column:
#' mean (sd), median, min–max, selected percentiles, and missingness counts.
#'
#' @param dataset A data frame containing \code{var_name}.
#' @param var_name String. The column name in \code{dataset} to summarize (must be numeric).
#' @param display_name String. Label to show in the \code{Variable} column.
#'   Defaults to \code{var_name}.
#' @param digits Integer. Number of decimal places for formatted outputs (default \code{2}).
#' @param finite_only Logical. If \code{TRUE} (default), exclude \code{Inf/-Inf} from summaries.
#' @param probs Numeric vector of quantile probabilities in the order you want reported.
#'   Defaults to \code{c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)}.
#'   Output names are generated from these probabilities; quartiles use
#'   \code{Q1}, \code{Q2}, and \code{Q3}, and other probabilities use
#'   percentage labels such as \code{P20}.
#'
#' @returns A one-row data frame with columns:
#' \itemize{
#'   \item \code{Variable}, \code{Mean_SD}, \code{Median}, \code{Min_Max},
#'   \item one column for each value in \code{probs},
#'   \item \code{Missing}, \code{N}, \code{NonMissing}, \code{MissingPct},
#'     \code{NonFinite}, and \code{Analyzed}.
#' }
#'
#' @details
#' If all values are missing (or filtered out by \code{finite_only = TRUE}),
#' numeric summaries are returned as \code{NA_character_} while counts are provided.
#' \code{NonMissing} counts all non-missing values, \code{NonFinite} counts
#' infinite values, and \code{Analyzed} reports the number used in summaries.
#'
#' @examples
#' x <- data.frame(a = c(rnorm(100), NA, Inf))
#' BD_1_cont(x, "a", display_name = "My Var")
#' BD_1_cont(x, "a", probs = c(0.20, 0.80))
#'
#' @export
BD_1_cont <- function(dataset,
                      var_name,
                      display_name = var_name,
                      digits = 2,
                      finite_only = TRUE,
                      probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)) {
  validated <- moose_validate_summary_inputs(
    dataset,
    var_name,
    display_name,
    digits
  )
  var_name <- validated$var_name
  display_name <- validated$display_name
  digits <- validated$digits
  finite_only <- moose_validate_single_logical(finite_only, "finite_only")
  probs <- moose_validate_probs(probs)
  quantile_labels <- moose_quantile_labels(probs)

  x <- dataset[[var_name]]
  if (!is.numeric(x)) {
    stop("Variable must be numeric: ", var_name, call. = FALSE)
  }

  # --- Missing / finite handling ---
  na_idx <- is.na(x)
  missing_n <- sum(na_idx)
  non_missing_n <- sum(!na_idx)
  non_finite_n <- sum(!na_idx & !is.finite(x))
  if (finite_only) {
    ok <- !is.na(x) & is.finite(x)
  } else {
    ok <- !is.na(x)
  }
  x_ok <- x[ok]
  analyzed_n <- length(x_ok)
  total_n <- length(x)
  missing_pct <- if (total_n == 0L) {
    NA_real_
  } else {
    round(100 * missing_n / total_n, 1)
  }

  build_result <- function(mean_sd,
                           median,
                           min_max,
                           quantiles) {
    row <- c(
      list(
        Variable = display_name,
        Mean_SD = mean_sd,
        Median = median,
        Min_Max = min_max
      ),
      stats::setNames(as.list(quantiles), quantile_labels),
      list(
        Missing = missing_n,
        N = total_n,
        NonMissing = non_missing_n,
        MissingPct = missing_pct,
        NonFinite = non_finite_n,
        Analyzed = analyzed_n
      )
    )

    data.frame(
      row,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  # Early return if nothing to summarize
  if (analyzed_n == 0L) {
    return(build_result(
      mean_sd = NA_character_,
      median = NA_character_,
      min_max = NA_character_,
      quantiles = rep(NA_character_, length(probs))
    ))
  }

  # --- Core summaries ---
  m   <- base::mean(x_ok)
  sdv <- stats::sd(x_ok)
  med <- stats::median(x_ok)
  mn  <- base::min(x_ok)
  mx  <- base::max(x_ok)

  # Quantiles in requested order
  qs <- stats::quantile(x_ok, probs = probs, names = FALSE, type = 7)

  # Formatter
  fmt <- function(v) sprintf(paste0("%.", digits, "f"), v)

  build_result(
    mean_sd = sprintf("%s (%s)", fmt(m), fmt(sdv)),
    median = fmt(med),
    min_max = sprintf("%s - %s", fmt(mn), fmt(mx)),
    quantiles = fmt(qs)
  )
}
