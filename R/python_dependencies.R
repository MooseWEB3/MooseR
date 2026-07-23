# Python dependency configuration used by the name-masking helpers.

.name_masking_state <- new.env(parent = emptyenv())
.name_masking_state$setup_complete <- FALSE
.name_masking_state$engine <- "none"
.name_masking_state$spacy <- NULL
.name_masking_state$model <- NULL
.name_masking_state$setup_error <- NULL

get_name_masking_state <- function() {
  .name_masking_state
}

declare_name_masking_dependencies <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "The reticulate package is required for spaCy-based name masking. ",
      "Install it with install.packages(\"reticulate\"), or use engine = \"regex\".",
      call. = FALSE
    )
  }

  reticulate::py_require(
    packages = c(
      "spacy>=3.8,<4",
      paste0(
        "en-core-web-sm @ ",
        "https://github.com/explosion/spacy-models/releases/",
        "download/en_core_web_sm-3.8.0/",
        "en_core_web_sm-3.8.0-py3-none-any.whl"
      )
    ),
    python_version = ">=3.10,<3.14"
  )

  invisible(TRUE)
}

set_name_masking_regex_engine <- function(error = NULL) {
  state <- get_name_masking_state()
  state$setup_complete <- TRUE
  state$engine <- "regex"
  state$spacy <- NULL
  state$model <- NULL
  state$setup_error <- error
  invisible(TRUE)
}
