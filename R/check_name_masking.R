#' Check the personal-name masking environment
#'
#' Reports the name-masking engine and, when available, Python/spaCy
#' configuration currently used by the package. By default, this checks the
#' pure R regex engine so locked-down computers do not need to initialize
#' Python.
#'
#' @param setup Logical. If \code{TRUE}, run
#'   \code{setup_name_masking(engine = engine)} before reporting status.
#' @param engine Character. One of \code{"auto"}, \code{"spacy"}, or
#'   \code{"regex"}.
#' @param verbose Logical. If \code{TRUE}, print stored setup errors for
#'   fallback mode.
#'
#' @return Invisibly returns a list containing environment information.
#'
#' @export
check_name_masking <- function(setup = TRUE,
                               engine = c("regex", "auto", "spacy"),
                               verbose = FALSE) {
  if (!is.logical(setup) || length(setup) != 1L || is.na(setup)) {
    stop("`setup` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  engine <- match.arg(engine)

  setup_error <- NULL

  if (isTRUE(setup)) {
    tryCatch(
      setup_name_masking(engine = engine),
      error = function(e) {
        setup_error <<- conditionMessage(e)
      }
    )
  }

  state <- get_name_masking_state()

  python_info <- tryCatch(
    {
      if (
        requireNamespace("reticulate", quietly = TRUE) &&
          identical(state$engine, "spacy")
      ) {
        reticulate::py_config()
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )

  result <- list(
    ready = isTRUE(state$setup_complete),
    engine = state$engine,
    python = if (!is.null(python_info)) {
      python_info$python
    } else {
      NA_character_
    },
    python_version = if (!is.null(python_info)) {
      as.character(python_info$version)
    } else {
      NA_character_
    },
    spacy_available = tryCatch(
      {
        if (
          requireNamespace("reticulate", quietly = TRUE) &&
            identical(state$engine, "spacy")
        ) {
          reticulate::py_module_available("spacy")
        } else {
          FALSE
        }
      },
      error = function(e) FALSE
    ),
    model_loaded = !is.null(state$model),
    error = if (!is.null(setup_error)) setup_error else state$setup_error
  )

  cat("Name-masking environment\n")
  cat("------------------------\n")
  cat("Ready:          ", result$ready, "\n", sep = "")
  cat("Engine:         ", result$engine, "\n", sep = "")
  if (identical(result$engine, "regex")) {
    cat("Python:         not used by regex engine\n")
    cat("Python version: not used by regex engine\n")
    cat("spaCy available:not checked by regex engine\n")
    cat("Model loaded:   not needed by regex engine\n")
  } else {
    cat("Python:         ", result$python, "\n", sep = "")
    cat("Python version: ", result$python_version, "\n", sep = "")
    cat("spaCy available:", result$spacy_available, "\n")
    cat("Model loaded:   ", result$model_loaded, "\n", sep = "")
  }

  if (!is.null(result$error) && (isTRUE(verbose) || identical(result$engine, "spacy"))) {
    cat("\nError:\n")
    cat(result$error, "\n")
  }

  invisible(result)
}
