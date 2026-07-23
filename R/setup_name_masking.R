#' Set up the personal-name masking environment
#'
#' Prepares the personal-name masking engine. By default, MooseR tries to use
#' spaCy through reticulate and falls back to a pure R regular-expression engine
#' if Python cannot be initialized.
#'
#' The spaCy engine may require internet access on the first run. The regex
#' engine does not require Python and is intended for locked-down work
#' computers where Python cannot be changed.
#'
#' @param force Logical. If \code{TRUE}, perform the setup test even if the
#'   current R session has already initialized the model.
#' @param engine Character. One of \code{"auto"}, \code{"spacy"}, or
#'   \code{"regex"}. \code{"auto"} tries spaCy and falls back to regex.
#'
#' @return Invisibly returns \code{TRUE} when setup succeeds.
#'
#' @export
setup_name_masking <- function(force = FALSE,
                               engine = c("auto", "spacy", "regex")) {
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {
    stop("`force` must be TRUE or FALSE.", call. = FALSE)
  }

  engine <- match.arg(engine)
  state <- get_name_masking_state()

  if (
    isTRUE(state$setup_complete) &&
      !isTRUE(force) &&
      (engine == "auto" || identical(state$engine, engine))
  ) {
    message("The name-masking environment is already ready.")
    return(invisible(TRUE))
  }

  if (engine == "regex") {
    set_name_masking_regex_engine()
    message("Name masking is ready using the pure R regex engine.")
    return(invisible(TRUE))
  }

  message("Preparing the Python and spaCy environment...")

  setup_error <- NULL

  spacy_setup <- tryCatch(
    {
      declare_name_masking_dependencies()

      spacy <- reticulate::import(
        module = "spacy",
        delay_load = FALSE,
        convert = FALSE
      )

      model <- spacy$load(
        "en_core_web_sm",
        disable = reticulate::tuple(
          "parser",
          "lemmatizer",
          "textcat"
        )
      )

      list(spacy = spacy, model = model)
    },
    error = function(e) {
      setup_error <<- paste0(
        "spaCy could not be initialized.\n\n",
        "Original error:\n",
        conditionMessage(e),
        "\n\n",
        "Possible causes include:\n",
        "- reticulate is not installed;\n",
        "- the computer cannot access the Python package source;\n",
        "- corporate proxy or firewall restrictions;\n",
        "- security software blocked Python;\n",
        "- the R session already loaded a locked or incompatible Python.\n\n",
        "Use setup_name_masking(engine = \"regex\") on locked-down computers, ",
        "or restart R and try setup_name_masking(engine = \"spacy\") again."
      )
      NULL
    }
  )

  if (is.null(spacy_setup)) {
    if (engine == "spacy") {
      stop(setup_error, call. = FALSE)
    }

    set_name_masking_regex_engine(setup_error)
    message(
      paste0(
        "spaCy is not available, so MooseR will use the pure R regex engine. ",
        "Run check_name_masking() to see details."
      )
    )
    return(invisible(TRUE))
  }

  test_doc <- spacy_setup$model("John Smith met Sarah Johnson in Vancouver.")

  test_ok <- tryCatch(
    {
      entities <- reticulate::iterate(
        test_doc$ents,
        simplify = FALSE
      )

      labels <- vapply(
        entities,
        function(entity) {
          reticulate::py_to_r(entity$label_)
        },
        character(1)
      )

      any(labels == "PERSON")
    },
    error = function(e) {
      setup_error <<- paste0(
        "spaCy loaded, but the setup test failed.\n\n",
        "Original error:\n",
        conditionMessage(e)
      )
      FALSE
    }
  )

  if (!isTRUE(test_ok)) {
    if (engine == "spacy") {
      stop(setup_error, call. = FALSE)
    }

    set_name_masking_regex_engine(setup_error)
    warning(
      paste0(
        "spaCy loaded, but the setup test did not succeed. ",
        "MooseR will use the pure R regex engine."
      ),
      call. = FALSE
    )
    return(invisible(TRUE))
  }

  state$spacy <- spacy_setup$spacy
  state$model <- spacy_setup$model
  state$setup_complete <- TRUE
  state$engine <- "spacy"
  state$setup_error <- NULL

  message("Name masking is ready using spaCy.")

  invisible(TRUE)
}
