moose_validate_summary_inputs <- function(dataset,
                                          var_name,
                                          display_name,
                                          digits) {
  if (!is.data.frame(dataset)) {
    stop("`dataset` must be a data frame.", call. = FALSE)
  }

  if (
    !is.character(var_name) ||
      length(var_name) != 1L ||
      is.na(var_name) ||
      !nzchar(var_name)
  ) {
    stop("`var_name` must be one non-empty character value.", call. = FALSE)
  }

  if (!var_name %in% names(dataset)) {
    stop("Variable not found in dataset: ", var_name, call. = FALSE)
  }

  if (
    !is.character(display_name) ||
      length(display_name) != 1L ||
      is.na(display_name) ||
      !nzchar(display_name)
  ) {
    stop("`display_name` must be one non-empty character value.", call. = FALSE)
  }

  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      !is.finite(digits) ||
      digits < 0 ||
      digits != floor(digits)
  ) {
    stop("`digits` must be one non-negative integer.", call. = FALSE)
  }

  list(
    var_name = var_name,
    display_name = display_name,
    digits = as.integer(digits)
  )
}

moose_validate_summary_label <- function(x, arg) {
  if (
    !is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !nzchar(x)
  ) {
    stop("`", arg, "` must be one non-empty character value.", call. = FALSE)
  }

  x
}

moose_validate_top_n <- function(top_n) {
  if (is.null(top_n)) {
    return(NULL)
  }

  if (
    !is.numeric(top_n) ||
      length(top_n) != 1L ||
      is.na(top_n) ||
      !is.finite(top_n) ||
      top_n < 1 ||
      top_n != floor(top_n) ||
      top_n > .Machine$integer.max
  ) {
    stop("`top_n` must be NULL or one positive integer.", call. = FALSE)
  }

  as.integer(top_n)
}

moose_validate_probs <- function(probs) {
  if (
    !is.numeric(probs) ||
      length(probs) == 0L ||
      anyNA(probs) ||
      any(!is.finite(probs)) ||
      any(probs < 0 | probs > 1)
  ) {
    stop(
      "`probs` must contain finite, non-missing numbers between 0 and 1.",
      call. = FALSE
    )
  }

  if (anyDuplicated(probs)) {
    stop("`probs` must not contain duplicates.", call. = FALSE)
  }

  as.numeric(probs)
}

moose_quantile_labels <- function(probs) {
  tolerance <- sqrt(.Machine$double.eps)

  labels <- vapply(
    probs,
    function(prob) {
      if (abs(prob - 0.25) < tolerance) {
        return("Q1")
      }
      if (abs(prob - 0.50) < tolerance) {
        return("Q2")
      }
      if (abs(prob - 0.75) < tolerance) {
        return("Q3")
      }

      percentage <- format(
        100 * prob,
        trim = TRUE,
        scientific = FALSE,
        digits = 12
      )
      paste0("P", percentage)
    },
    character(1)
  )

  make.unique(labels, sep = "_")
}
