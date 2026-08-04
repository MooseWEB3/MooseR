# Alberta Education School Information Report:
# https://education.alberta.ca/media/1626669/authority_and_school.xlsx
#
# The bundled UTF-8 snapshot is updated with
# data-raw/update-alberta-schools.ps1. It contains 2,616 unique school names
# from 2,679 current entries in the July 8, 2026 report.
alberta_schools <- local({
  schools <- NULL

  function() {
    if (!is.null(schools)) {
      return(schools)
    }

    path <- system.file(
      "extdata",
      "alberta-schools.txt",
      package = "MooseR"
    )

    if (!nzchar(path)) {
      source_path <- file.path("inst", "extdata", "alberta-schools.txt")

      if (file.exists(source_path)) {
        path <- source_path
      }
    }

    if (!nzchar(path) || !file.exists(path)) {
      stop("The bundled Alberta school registry is missing.", call. = FALSE)
    }

    schools <<- readLines(path, encoding = "UTF-8", warn = FALSE)
    schools <<- unique(schools[nzchar(trimws(schools))])
    schools
  }
})

normalize_alberta_school <- function(x) {
  transliterated <- suppressWarnings(iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT",
    sub = ""
  ))
  fallback <- is.na(transliterated) & !is.na(x)
  transliterated[fallback] <- x[fallback]
  transliterated <- tolower(transliterated)
  transliterated <- gsub("&", " and ", transliterated, fixed = TRUE)
  transliterated <- gsub("[^a-z0-9]+", " ", transliterated, perl = TRUE)
  trimws(gsub("\\s+", " ", transliterated, perl = TRUE))
}

alberta_school_names_normalized <- local({
  normalized <- NULL

  function() {
    if (is.null(normalized)) {
      normalized <<- unique(normalize_alberta_school(alberta_schools()))
      normalized <<- normalized[nzchar(normalized)]
    }

    normalized
  }
})

alberta_school_fragment_index <- local({
  index <- NULL

  function() {
    if (!is.null(index)) {
      return(index)
    }

    school_fragments <- lapply(
      alberta_school_names_normalized(),
      function(school) {
        words <- strsplit(school, " ", fixed = TRUE)[[1]]
        max_size <- min(3L, length(words))
        fragments <- unlist(
          lapply(
            seq_len(max_size),
            function(size) {
              starts <- seq_len(length(words) - size + 1L)
              vapply(
                starts,
                function(start) {
                  paste(words[seq.int(start, start + size - 1L)], collapse = " ")
                },
                character(1)
              )
            }
          ),
          use.names = FALSE
        )

        list(
          fragments = unique(fragments),
          school = school
        )
      }
    )
    fragment_names <- unlist(
      lapply(school_fragments, `[[`, "fragments"),
      use.names = FALSE
    )
    school_values <- unlist(
      lapply(
        school_fragments,
        function(value) rep.int(value$school, length(value$fragments))
      ),
      use.names = FALSE
    )
    index <<- split(school_values, fragment_names)
    index <<- lapply(index, unique)
    index
  }
})

is_alberta_school_candidate_values <- function(text, candidate) {
  if (length(text) != length(candidate)) {
    stop("`text` and `candidate` must have the same length.", call. = FALSE)
  }

  raw_candidate <- normalize_alberta_school(candidate)
  candidate <- sub(
    "(?i)^(?:Mr|Mrs|Ms|Miss|Dr|Doctor|RN|Paramedic|EMT)\\.?\\s+",
    "",
    candidate,
    perl = TRUE
  )
  candidate <- normalize_alberta_school(candidate)
  result <- rep.int(FALSE, length(candidate))

  if (!length(candidate)) {
    return(result)
  }

  valid <- !is.na(candidate) & nzchar(candidate)
  exact <- !is.na(raw_candidate) & nzchar(raw_candidate) &
    raw_candidate %in% alberta_school_names_normalized()
  result[exact] <- TRUE
  partial <- which(valid & !exact)

  if (!length(partial)) {
    return(result)
  }

  index <- alberta_school_fragment_index()
  possible <- candidate[partial] %in% names(index)
  partial <- partial[possible]

  if (!length(partial)) {
    return(result)
  }

  normalized_text <- normalize_alberta_school(text[partial])
  pair_keys <- paste(candidate[partial], normalized_text, sep = "\034")
  unique_pairs <- !duplicated(pair_keys)
  unique_positions <- partial[unique_pairs]
  unique_text <- paste0(" ", normalized_text[unique_pairs], " ")
  unique_result <- mapply(
    function(key, value) {
      schools <- index[[key]]

      any(vapply(
        schools,
        function(school) {
          grepl(paste0(" ", school, " "), value, fixed = TRUE)
        },
        logical(1)
      ))
    },
    key = candidate[unique_positions],
    value = unique_text,
    USE.NAMES = FALSE
  )
  result[partial] <- unique_result[match(pair_keys, pair_keys[unique_pairs])]
  result
}

is_alberta_school_candidate_value <- function(text, candidate) {
  is_alberta_school_candidate_values(text, candidate)[1]
}

is_alberta_school_candidate <- function(text, start, end) {
  is_alberta_school_candidate_value(text, substr(text, start, end))
}
