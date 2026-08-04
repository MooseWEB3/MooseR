validate_known_name_column_inputs <- function(text, data, name_columns) {
  supplied <- !is.null(data) || !is.null(name_columns)

  if (!supplied) {
    return(FALSE)
  }

  if (is.null(data) || is.null(name_columns)) {
    stop(
      "`data` and `name_columns` must be supplied together.",
      call. = FALSE
    )
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (
    !is.character(name_columns) ||
      length(name_columns) == 0L ||
      anyNA(name_columns) ||
      any(!nzchar(name_columns))
  ) {
    stop(
      "`name_columns` must be a non-empty character vector of column names.",
      call. = FALSE
    )
  }

  missing_columns <- setdiff(name_columns, names(data))

  if (length(missing_columns)) {
    stop(
      paste0(
        "Unknown `name_columns`: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (nrow(data) != length(text)) {
    stop(
      "`data` must have the same number of rows as the length of `text`.",
      call. = FALSE
    )
  }

  valid_types <- vapply(
    data[unique(name_columns)],
    function(value) is.character(value) || is.factor(value),
    logical(1)
  )

  if (!all(valid_types)) {
    stop(
      "Every `name_columns` column must be character or factor.",
      call. = FALSE
    )
  }

  TRUE
}

known_name_missing_values <- function() {
  c(
    "N/A", "NA", "NONE", "NULL", "UNKNOWN", "UNK",
    "NOT APPLICABLE", "NOT AVAILABLE", "NOT PROVIDED",
    "ANONYMOUS", "UNIDENTIFIED"
  )
}

normalize_known_name_values <- function(value) {
  value <- as.character(value)
  value <- trimws(value)
  value <- gsub("[[:space:]]+", " ", value, perl = TRUE)
  value
}

is_usable_known_name_value <- function(value) {
  letters <- nchar(gsub("[^[:alpha:]]", "", value, perl = TRUE))

  !is.na(value) &
    nzchar(value) &
    letters >= 2L &
    !(toupper(value) %in% known_name_missing_values())
}

escape_known_name_regex <- function(value) {
  value <- gsub(
    "([][{}()+*^$|\\\\?.])",
    "\\\\\\1",
    value,
    perl = TRUE
  )
  gsub("[[:space:]]+", "\\\\s+", value, perl = TRUE)
}

known_name_patterns_for_rows <- function(data, name_columns, rows) {
  name_columns <- unique(name_columns)
  values <- lapply(
    data[rows, name_columns, drop = FALSE],
    normalize_known_name_values
  )
  usable <- lapply(values, is_usable_known_name_value)
  any_usable <- Reduce(`|`, usable)
  escaped <- Map(
    function(value, keep) {
      output <- rep("(?!)", length(value))
      output[keep] <- escape_known_name_regex(value[keep])
      output
    },
    values,
    usable
  )

  individual <- do.call(
    paste,
    c(escaped, list(sep = "|"))
  )
  all_usable <- Reduce(`&`, usable)
  ordered <- rep("(?!)", length(rows))
  ordered[all_usable] <- do.call(
    paste,
    c(lapply(escaped, `[`, all_usable), list(sep = "\\s+"))
  )

  comma_ordered <- rep("(?!)", length(rows))

  if (length(name_columns) >= 2L && any(all_usable)) {
    last_value <- escaped[[length(escaped)]][all_usable]
    preceding <- do.call(
      paste,
      c(
        lapply(escaped[-length(escaped)], `[`, all_usable),
        list(sep = "\\s+")
      )
    )
    comma_ordered[all_usable] <- paste0(
      last_value,
      "\\s*,\\s*",
      preceding
    )
  }

  patterns <- rep(NA_character_, length(rows))
  patterns[any_usable] <- paste0(
    "(?<![[:alnum:]_'-])(?:",
    ordered[any_usable],
    "|",
    comma_ordered[any_usable],
    "|",
    individual[any_usable],
    ")(?![[:alnum:]_'-])"
  )
  patterns
}

known_name_chunk_rows <- function(text, chunk_size = 50000L) {
  valid_rows <- which(!is.na(text) & nzchar(trimws(text)))

  if (!length(valid_rows)) {
    return(list())
  }

  split(
    valid_rows,
    ceiling(seq_along(valid_rows) / as.integer(chunk_size))
  )
}

mask_known_name_columns <- function(text,
                                    replacement,
                                    data,
                                    name_columns) {
  output <- text

  for (rows in known_name_chunk_rows(text)) {
    patterns <- known_name_patterns_for_rows(data, name_columns, rows)
    matchable <- !is.na(patterns)

    if (!any(matchable)) {
      next
    }

    target_rows <- rows[matchable]
    target_patterns <- patterns[matchable]
    unique_patterns <- unique(target_patterns)

    if (length(unique_patterns) < length(target_patterns)) {
      groups <- split(
        seq_along(target_rows),
        match(target_patterns, unique_patterns)
      )

      for (group_id in names(groups)) {
        group <- groups[[group_id]]
        group_rows <- target_rows[group]
        output[group_rows] <- gsub(
          unique_patterns[as.integer(group_id)],
          replacement,
          output[group_rows],
          ignore.case = TRUE,
          perl = TRUE
        )
      }
    } else {
      output[target_rows] <- mapply(
        function(pattern, value) {
          gsub(
            pattern,
            replacement,
            value,
            ignore.case = TRUE,
            perl = TRUE
          )
        },
        target_patterns,
        output[target_rows],
        USE.NAMES = FALSE
      )
    }
  }

  output
}

flag_known_name_columns <- function(text, data, name_columns) {
  flag <- integer(length(text))

  for (rows in known_name_chunk_rows(text)) {
    patterns <- known_name_patterns_for_rows(data, name_columns, rows)
    matchable <- !is.na(patterns)

    if (!any(matchable)) {
      next
    }

    target_rows <- rows[matchable]
    target_patterns <- patterns[matchable]
    unique_patterns <- unique(target_patterns)

    if (length(unique_patterns) < length(target_patterns)) {
      groups <- split(
        seq_along(target_rows),
        match(target_patterns, unique_patterns)
      )

      for (group_id in names(groups)) {
        group <- groups[[group_id]]
        group_rows <- target_rows[group]
        flag[group_rows] <- as.integer(grepl(
          unique_patterns[as.integer(group_id)],
          text[group_rows],
          ignore.case = TRUE,
          perl = TRUE
        ))
      }
    } else {
      flag[target_rows] <- as.integer(mapply(
        function(pattern, value) {
          grepl(
            pattern,
            value,
            ignore.case = TRUE,
            perl = TRUE
          )
        },
        target_patterns,
        text[target_rows],
        USE.NAMES = FALSE
      ))
    }
  }

  flag
}

validate_known_name_similarity_inputs <- function(max_distance, min_chars) {
  if (
    length(max_distance) != 1L ||
      is.na(max_distance) ||
      !is.numeric(max_distance) ||
      max_distance != as.integer(max_distance) ||
      max_distance < 0L ||
      max_distance > 3L
  ) {
    stop("`max_distance` must be one integer from 0 to 3.", call. = FALSE)
  }

  if (
    length(min_chars) != 1L ||
      is.na(min_chars) ||
      !is.numeric(min_chars) ||
      min_chars != as.integer(min_chars) ||
      min_chars < 2L
  ) {
    stop("`min_chars` must be one integer of at least 2.", call. = FALSE)
  }

  invisible(TRUE)
}

known_name_fuzzy_token_pattern <- function() {
  "[[:alpha:]]+(?:[-'][[:alpha:]]+)*"
}

normalize_known_name_fuzzy_token <- function(value) {
  transliterated <- suppressWarnings(iconv(
    value,
    from = "",
    to = "ASCII//TRANSLIT",
    sub = ""
  ))
  fallback <- is.na(transliterated) & !is.na(value)
  transliterated[fallback] <- value[fallback]
  transliterated <- tolower(transliterated)
  gsub("[^a-z]", "", transliterated, perl = TRUE)
}

extract_known_name_fuzzy_tokens <- function(value) {
  if (is.na(value) || !nzchar(value)) {
    return(character())
  }

  matches <- gregexpr(
    known_name_fuzzy_token_pattern(),
    value,
    perl = TRUE
  )[[1]]

  if (identical(matches[1], -1L)) {
    return(character())
  }

  tokens <- regmatches(value, list(matches))[[1]]
  unique(normalize_known_name_fuzzy_token(tokens))
}

known_name_fuzzy_tokens_for_rows <- function(data,
                                             name_columns,
                                             rows,
                                             min_chars) {
  name_columns <- unique(name_columns)
  values <- lapply(
    data[rows, name_columns, drop = FALSE],
    normalize_known_name_values
  )
  row_keys <- do.call(
    paste,
    c(lapply(values, function(value) ifelse(is.na(value), "", value)),
      list(sep = "\034"))
  )
  unique_rows <- !duplicated(row_keys)
  unique_positions <- which(unique_rows)
  unique_tokens <- lapply(
    unique_positions,
    function(position) {
      row_values <- vapply(
        values,
        function(value) value[position],
        character(1)
      )
      usable <- is_usable_known_name_value(row_values)

      if (!any(usable)) {
        return(character())
      }

      tokens <- unique(unlist(
        lapply(row_values[usable], extract_known_name_fuzzy_tokens),
        use.names = FALSE
      ))
      tokens[nchar(tokens) >= min_chars]
    }
  )

  unique_tokens[match(row_keys, row_keys[unique_rows])]
}

known_name_fuzzy_matches <- function(text,
                                     known_tokens,
                                     max_distance,
                                     min_chars,
                                     excluded_tokens = character()) {
  empty <- data.frame(start = integer(), end = integer())

  if (
    is.na(text) ||
      !nzchar(trimws(text)) ||
      !length(known_tokens)
  ) {
    return(empty)
  }

  matches <- gregexpr(
    known_name_fuzzy_token_pattern(),
    text,
    perl = TRUE
  )[[1]]

  if (identical(matches[1], -1L)) {
    return(empty)
  }

  tokens <- regmatches(text, list(matches))[[1]]
  token_keys <- normalize_known_name_fuzzy_token(tokens)
  candidate_min_chars <- max(2L, min_chars - max_distance)
  eligible <-
    nchar(token_keys) >= candidate_min_chars &
    !(token_keys %in% excluded_tokens)

  if (!any(eligible)) {
    return(empty)
  }

  unique_keys <- unique(token_keys[eligible])
  distances <- utils::adist(unique_keys, known_tokens)
  matched_keys <- unique_keys[apply(
    distances,
    1L,
    function(distance) any(distance <= max_distance)
  )]
  keep <- eligible & token_keys %in% matched_keys

  if (!any(keep)) {
    return(empty)
  }

  data.frame(
    start = as.integer(matches[keep]),
    end = as.integer(matches[keep] + attr(matches, "match.length")[keep] - 1L)
  )
}

mask_similar_known_name_value <- function(text,
                                          known_tokens,
                                          replacement,
                                          max_distance,
                                          min_chars) {
  replacement_tokens <- extract_known_name_fuzzy_tokens(replacement)
  positions <- known_name_fuzzy_matches(
    text = text,
    known_tokens = known_tokens,
    max_distance = max_distance,
    min_chars = min_chars,
    excluded_tokens = replacement_tokens
  )

  if (!nrow(positions)) {
    return(text)
  }

  positions <- positions[order(positions$start, decreasing = TRUE), , drop = FALSE]
  output <- text

  for (i in seq_len(nrow(positions))) {
    left <- if (positions$start[i] > 1L) {
      substr(output, 1L, positions$start[i] - 1L)
    } else {
      ""
    }
    right <- if (positions$end[i] < nchar(output)) {
      substr(output, positions$end[i] + 1L, nchar(output))
    } else {
      ""
    }
    output <- paste0(left, replacement, right)
  }

  output
}

mask_similar_known_name_columns <- function(text,
                                            replacement,
                                            data,
                                            name_columns,
                                            max_distance,
                                            min_chars) {
  output <- text

  for (rows in known_name_chunk_rows(text)) {
    known_tokens <- known_name_fuzzy_tokens_for_rows(
      data = data,
      name_columns = name_columns,
      rows = rows,
      min_chars = min_chars
    )
    token_keys <- vapply(
      known_tokens,
      paste,
      collapse = "\035",
      FUN.VALUE = character(1)
    )
    pair_keys <- paste(output[rows], token_keys, sep = "\034")
    unique_pairs <- !duplicated(pair_keys)
    unique_positions <- which(unique_pairs)
    unique_output <- mapply(
      mask_similar_known_name_value,
      text = output[rows[unique_positions]],
      known_tokens = known_tokens[unique_positions],
      MoreArgs = list(
        replacement = replacement,
        max_distance = max_distance,
        min_chars = min_chars
      ),
      USE.NAMES = FALSE
    )
    output[rows] <- unique_output[match(pair_keys, pair_keys[unique_pairs])]
  }

  output
}

flag_similar_known_name_columns <- function(text,
                                            data,
                                            name_columns,
                                            max_distance,
                                            min_chars) {
  flag <- integer(length(text))

  for (rows in known_name_chunk_rows(text)) {
    known_tokens <- known_name_fuzzy_tokens_for_rows(
      data = data,
      name_columns = name_columns,
      rows = rows,
      min_chars = min_chars
    )
    token_keys <- vapply(
      known_tokens,
      paste,
      collapse = "\035",
      FUN.VALUE = character(1)
    )
    pair_keys <- paste(text[rows], token_keys, sep = "\034")
    unique_pairs <- !duplicated(pair_keys)
    unique_positions <- which(unique_pairs)
    unique_flag <- as.integer(mapply(
      function(value, tokens) {
        nrow(known_name_fuzzy_matches(
          text = value,
          known_tokens = tokens,
          max_distance = max_distance,
          min_chars = min_chars
        )) > 0L
      },
      value = text[rows[unique_positions]],
      tokens = known_tokens[unique_positions],
      USE.NAMES = FALSE
    ))
    flag[rows] <- unique_flag[match(pair_keys, pair_keys[unique_pairs])]
  }

  flag
}
