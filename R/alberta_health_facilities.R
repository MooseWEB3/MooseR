# Alberta Health Services, Find Healthcare facility directory:
# https://www.albertahealthservices.ca/findhealth/search.aspx
#
# The bundled UTF-8 snapshot is updated with
# data-raw/update-alberta-health-facilities.ps1. It contains every facility
# unique name offered by the AHS facility-name search as of July 31, 2026
# (1,150 unique names from 1,154 entries).
alberta_health_facility_names <- local({
  facilities <- NULL

  function() {
    if (!is.null(facilities)) {
      return(facilities)
    }

    path <- system.file(
      "extdata",
      "alberta-health-facilities.txt",
      package = "MooseR"
    )

    if (!nzchar(path)) {
      source_path <- file.path(
        "inst",
        "extdata",
        "alberta-health-facilities.txt"
      )

      if (file.exists(source_path)) {
        path <- source_path
      }
    }

    if (!nzchar(path) || !file.exists(path)) {
      stop("The bundled Alberta health-facility registry is missing.", call. = FALSE)
    }

    facilities <<- readLines(path, encoding = "UTF-8", warn = FALSE)
    facilities <<- unique(facilities[nzchar(trimws(facilities))])
    facilities
  }
})

normalize_alberta_health_facility <- function(x) {
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x, perl = TRUE))
}

alberta_health_facility_names_normalized <- local({
  normalized <- NULL

  function() {
    if (is.null(normalized)) {
      normalized <<- unique(normalize_alberta_health_facility(
        alberta_health_facility_names()
      ))
      normalized <<- normalized[nzchar(normalized)]
    }

    normalized
  }
})

alberta_health_facility_fragment_index <- local({
  index <- NULL

  function() {
    if (!is.null(index)) {
      return(index)
    }

    normalized <- alberta_health_facility_names_normalized()
    fragment_names <- character()
    facility_values <- character()

    for (facility in normalized) {
      words <- strsplit(facility, " ", fixed = TRUE)[[1]]
      max_size <- min(3L, length(words))

      for (size in seq_len(max_size)) {
        starts <- seq_len(length(words) - size + 1L)
        fragments <- vapply(
          starts,
          function(start) {
            paste(words[seq.int(start, start + size - 1L)], collapse = " ")
          },
          character(1)
        )
        fragment_names <- c(fragment_names, fragments)
        facility_values <- c(
          facility_values,
          rep.int(facility, length(fragments))
        )
      }
    }

    index <<- split(facility_values, fragment_names)
    index <<- lapply(index, unique)
    index
  }
})

is_alberta_health_facility_candidate_values <- function(text, candidate) {
  if (length(text) != length(candidate)) {
    stop("`text` and `candidate` must have the same length.", call. = FALSE)
  }

  candidate <- sub(
    "(?i)^(?:Mr|Mrs|Ms|Miss|Dr|Doctor|RN|Paramedic|EMT)\\.?\\s+",
    "",
    candidate,
    perl = TRUE
  )
  candidate <- normalize_alberta_health_facility(candidate)
  result <- rep.int(FALSE, length(candidate))

  if (!length(candidate)) {
    return(result)
  }

  index <- alberta_health_facility_fragment_index()
  possible <- !is.na(candidate) & nzchar(candidate) &
    candidate %in% names(index)

  if (!any(possible)) {
    return(result)
  }

  exact <- possible & candidate %in%
    alberta_health_facility_names_normalized()
  result[exact] <- TRUE
  partial <- which(possible & !exact)

  if (!length(partial)) {
    return(result)
  }

  normalized_text <- normalize_alberta_health_facility(text[partial])
  pair_keys <- paste(candidate[partial], normalized_text, sep = "\034")
  unique_pairs <- !duplicated(pair_keys)
  unique_positions <- partial[unique_pairs]
  unique_text <- paste0(
    " ",
    normalized_text[unique_pairs],
    " "
  )
  unique_result <- mapply(
    function(key, value) {
      facilities <- index[[key]]

      any(vapply(
        facilities,
        function(facility) {
          grepl(
            paste0(" ", facility, " "),
            value,
            fixed = TRUE
          )
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

is_alberta_health_facility_candidate_value <- function(text, candidate) {
  is_alberta_health_facility_candidate_values(text, candidate)[1]
}

is_alberta_health_facility_candidate <- function(text, start, end) {
  is_alberta_health_facility_candidate_value(
    text,
    substr(text, start, end)
  )
}
