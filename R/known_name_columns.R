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
