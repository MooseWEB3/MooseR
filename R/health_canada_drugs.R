# Health Canada Drug Product Database (DPD):
# https://www.canada.ca/en/health-canada/services/drugs-health-products/
# drug-products/drug-product-database/what-data-extract-drug-product-database.html
#
# The bundled UTF-8 snapshot is updated with
# data-raw/update-health-canada-drugs.ps1. It contains 12,435 unique English
# and French brand and active-ingredient names for marketed or approved human
# drugs in the August 4, 2026 extract.
health_canada_drugs <- local({
  drugs <- NULL

  function() {
    if (!is.null(drugs)) {
      return(drugs)
    }

    path <- system.file(
      "extdata",
      "health-canada-drugs.txt",
      package = "MooseR"
    )

    if (!nzchar(path)) {
      source_path <- file.path(
        "inst",
        "extdata",
        "health-canada-drugs.txt"
      )

      if (file.exists(source_path)) {
        path <- source_path
      }
    }

    if (!nzchar(path) || !file.exists(path)) {
      stop("The bundled Health Canada drug registry is missing.", call. = FALSE)
    }

    drugs <<- readLines(path, encoding = "UTF-8", warn = FALSE)
    drugs <<- unique(drugs[nzchar(trimws(drugs))])
    drugs
  }
})

normalize_health_canada_drug <- function(x) {
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

health_canada_drug_names_normalized <- local({
  normalized <- NULL

  function() {
    if (is.null(normalized)) {
      normalized <<- unique(normalize_health_canada_drug(
        health_canada_drugs()
      ))
      normalized <<- normalized[nzchar(normalized)]
    }

    normalized
  }
})

health_canada_drug_fragment_index <- local({
  index <- NULL

  function() {
    if (!is.null(index)) {
      return(index)
    }

    drug_fragments <- lapply(
      health_canada_drug_names_normalized(),
      function(drug) {
        words <- strsplit(drug, " ", fixed = TRUE)[[1]]
        max_size <- min(4L, length(words))
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

        list(fragments = unique(fragments), drug = drug)
      }
    )
    fragment_names <- unlist(
      lapply(drug_fragments, `[[`, "fragments"),
      use.names = FALSE
    )
    drug_values <- unlist(
      lapply(
        drug_fragments,
        function(value) rep.int(value$drug, length(value$fragments))
      ),
      use.names = FALSE
    )
    index <<- split(drug_values, fragment_names)
    index <<- lapply(index, unique)
    index
  }
})

is_health_canada_drug_candidate_values <- function(text, candidate) {
  if (length(text) != length(candidate)) {
    stop("`text` and `candidate` must have the same length.", call. = FALSE)
  }

  raw_candidate <- normalize_health_canada_drug(candidate)
  candidate <- sub(
    "(?i)^(?:Mr|Mrs|Ms|Miss|Dr|Doctor|RN|Paramedic|EMT)\\.?\\s+",
    "",
    candidate,
    perl = TRUE
  )
  candidate <- normalize_health_canada_drug(candidate)
  result <- rep.int(FALSE, length(candidate))

  if (!length(candidate)) {
    return(result)
  }

  registered <- health_canada_drug_names_normalized()
  valid <- !is.na(candidate) & nzchar(candidate)
  exact <-
    (!is.na(raw_candidate) & nzchar(raw_candidate) & raw_candidate %in% registered) |
    (valid & candidate %in% registered)
  result[exact] <- TRUE
  partial <- which(valid & !exact)

  if (!length(partial)) {
    return(result)
  }

  index <- health_canada_drug_fragment_index()
  possible <- candidate[partial] %in% names(index)
  partial <- partial[possible]

  if (!length(partial)) {
    return(result)
  }

  normalized_text <- normalize_health_canada_drug(text[partial])
  pair_keys <- paste(candidate[partial], normalized_text, sep = "\034")
  unique_pairs <- !duplicated(pair_keys)
  unique_positions <- partial[unique_pairs]
  unique_text <- paste0(" ", normalized_text[unique_pairs], " ")
  unique_result <- mapply(
    function(key, value) {
      drugs <- index[[key]]

      any(vapply(
        drugs,
        function(drug) {
          grepl(paste0(" ", drug, " "), value, fixed = TRUE)
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

is_health_canada_drug_candidate_value <- function(text, candidate) {
  is_health_canada_drug_candidate_values(text, candidate)[1]
}

is_health_canada_drug_candidate <- function(text, start, end) {
  is_health_canada_drug_candidate_value(text, substr(text, start, end))
}
