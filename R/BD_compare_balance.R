#' Compare covariate balance between two data frames
#'
#' For each variable in `vars`, reports (a) for continuous variables:
#' weighted/unweighted mean, SD, N, missingness, difference, and optional SMD;
#' (b) for categorical variables: per-level proportions in each data set,
#' N, missingness, and difference in proportions.
#'
#' @param df1,df2 Data frames to compare.
#' @param vars Character vector of variable names to compare (can exist in either df).
#' @param weights1,weights2 Optional numeric weight vectors aligned with `df1`/`df2`.
#' @param digits Integer; number of decimal places for numeric outputs (default 4).
#' @param include_smd Logical; if TRUE, include standardized mean difference (continuous only).
#'
#' @return A data frame with one row per variable (continuous) or per level (categorical).
#' @examples
#' set.seed(1)
#' a <- data.frame(x=rnorm(100), g=sample(c("M","F"),100,TRUE), z=runif(100))
#' b <- data.frame(x=rnorm(120, .2, 1.1), g=sample(c("M","F"),120,TRUE), z=runif(120))
#' w1 <- runif(100); w2 <- runif(120)
#' BD_compare_balance(a,b,vars=c("x","g","z"), weights1=w1, weights2=w2, include_smd=TRUE)
#'
#' @export
BD_compare_balance <- function(df1, df2, vars, weights1 = NULL, weights2 = NULL,
                               digits = 4, include_smd = FALSE) {

  # --- helpers ---------------------------------------------------------------
  assert_weights <- function(w, n, label) {
    if (is.null(w)) return(invisible(TRUE))
    if (!is.numeric(w)) stop(label, " must be numeric or NULL.")
    if (length(w) != n) stop(label, " length (", length(w), ") must equal nrows (", n, ").")
    if (all(!is.finite(w))) stop(label, " has no finite values.")
    invisible(TRUE)
  }

  w_sum <- function(x, w = NULL, na.rm = TRUE) {
    if (is.null(w)) return(sum(x, na.rm = na.rm))
    ok <- is.finite(x) & is.finite(w)
    if (!any(ok)) return(NA_real_)
    sum(w[ok] * x[ok])
  }

  w_den <- function(x, w = NULL) {
    if (is.null(w)) return(sum(is.finite(x)))
    ok <- is.finite(x) & is.finite(w)
    if (!any(ok)) return(0)
    sum(w[ok])
  }

  w_mean <- function(x, w = NULL) {
    if (is.null(w)) return(mean(x, na.rm = TRUE))
    ok <- is.finite(x) & is.finite(w)
    if (!any(ok)) return(NA_real_)
    sw <- sum(w[ok])
    if (sw == 0) return(NA_real_)
    sum(w[ok] * x[ok]) / sw
  }

  # population-style weighted variance (matches your original intent)
  w_var <- function(x, w = NULL) {
    if (is.null(w)) return(stats::var(x, na.rm = TRUE))
    ok <- is.finite(x) & is.finite(w)
    x <- x[ok]; w <- w[ok]
    if (!length(x)) return(NA_real_)
    sw <- sum(w); if (sw == 0) return(NA_real_)
    mu <- sum(w * x) / sw
    sum(w * (x - mu)^2) / sw
  }

  # --- validate --------------------------------------------------------------
  if (!is.data.frame(df1)) stop("`df1` must be a data.frame.")
  if (!is.data.frame(df2)) stop("`df2` must be a data.frame.")
  if (!is.character(vars)) stop("`vars` must be a character vector of column names.")
  assert_weights(weights1, nrow(df1), "weights1")
  assert_weights(weights2, nrow(df2), "weights2")

  # var must exist in at least one df
  if (!all(vars %in% union(names(df1), names(df2)))) {
    missing_vars <- setdiff(vars, union(names(df1), names(df2)))
    stop("These vars are not found in either df1 or df2: ", paste(missing_vars, collapse = ", "))
  }

  out_list <- lapply(vars, function(v) {
    x1 <- if (v %in% names(df1)) df1[[v]] else rep(NA, nrow(df1))
    x2 <- if (v %in% names(df2)) df2[[v]] else rep(NA, nrow(df2))

    # treat characters as factors; logical as categorical (TRUE/FALSE levels)
    to_cat <- function(x) {
      if (is.logical(x)) return(factor(x, levels = c(FALSE, TRUE)))
      if (is.character(x) || is.factor(x) || is.ordered(x)) return(factor(x))
      NULL
    }
    level_union <- function(...) {
      xs <- list(...)
      lvls <- character()
      for (x in xs) {
        if (is.logical(x)) {
          lvls <- c(lvls, c("FALSE", "TRUE"))
        } else if (is.factor(x) || is.ordered(x)) {
          lvls <- c(lvls, as.character(levels(x)))
        } else {
          lvls <- c(lvls, as.character(stats::na.omit(unique(x))))
        }
      }
      unique(lvls)
    }
    make_row <- function(variable, type, level,
                         prop1, prop2, diff,
                         n1, mean1, sd1,
                         n2, mean2, sd2,
                         smd = NULL,
                         miss_n1, miss_pct1, miss_n2, miss_pct2) {
      row <- data.frame(
        variable = variable, type = type, level = level,
        prop1 = prop1, prop2 = prop2, diff = diff,
        n1 = n1, mean1 = mean1, sd1 = sd1,
        n2 = n2, mean2 = mean2, sd2 = sd2,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      if (include_smd) row$smd <- smd
      row$miss_n1 <- miss_n1
      row$miss_pct1 <- miss_pct1
      row$miss_n2 <- miss_n2
      row$miss_pct2 <- miss_pct2
      row
    }

    total1 <- length(x1); total2 <- length(x2)
    miss_n1 <- sum(is.na(x1)); miss_n2 <- sum(is.na(x2))
    miss_pct1 <- 100 * miss_n1 / total1
    miss_pct2 <- 100 * miss_n2 / total2

    cat1 <- to_cat(x1); cat2 <- to_cat(x2)

    if (is.null(cat1) && is.null(cat2) && is.numeric(x1) && is.numeric(x2)) {
      # continuous
      n1 <- sum(is.finite(x1)); n2 <- sum(is.finite(x2))
      m1 <- w_mean(x1, weights1); m2 <- w_mean(x2, weights2)
      sd1 <- sqrt(w_var(x1, weights1)); sd2 <- sqrt(w_var(x2, weights2))
      diff <- m2 - m1
      smd <- if (include_smd) {
        sp <- sqrt((sd1^2 + sd2^2) / 2)
        ifelse(is.finite(sp) && sp > 0, diff / sp, NA_real_)
      } else NA_real_

      make_row(
        variable = v, type = "continuous", level = NA_character_,
        prop1 = NA_real_, prop2 = NA_real_,
        diff = ifelse(is.na(diff), NA_real_, diff),
        n1 = n1, mean1 = m1, sd1 = sd1,
        n2 = n2, mean2 = m2, sd2 = sd2,
        smd = smd,
        miss_n1 = miss_n1, miss_pct1 = miss_pct1,
        miss_n2 = miss_n2, miss_pct2 = miss_pct2
      )

    } else {
      # categorical
      lvls <- level_union(x1, x2)
      f1 <- factor(x1, levels = lvls)
      f2 <- factor(x2, levels = lvls)

      # unweighted proportion or weighted proportion
      w_prop <- function(f, w = NULL, L) {
        if (is.null(w)) return(mean(f == L, na.rm = TRUE))
        ok <- !is.na(f) & is.finite(w)
        if (!any(ok)) return(NA_real_)
        sum(w[ok] * as.numeric(f[ok] == L)) / sum(w[ok])
      }

      n1 <- sum(!is.na(f1)); n2 <- sum(!is.na(f2))

      rows <- if (length(lvls) == 0L) {
        list(make_row(
          variable = v, type = "categorical", level = NA_character_,
          prop1 = NA_real_, prop2 = NA_real_, diff = NA_real_,
          n1 = n1, mean1 = NA_real_, sd1 = NA_real_,
          n2 = n2, mean2 = NA_real_, sd2 = NA_real_,
          smd = NA_real_,
          miss_n1 = miss_n1, miss_pct1 = miss_pct1,
          miss_n2 = miss_n2, miss_pct2 = miss_pct2
        ))
      } else {
        lapply(lvls, function(L) {
          p1 <- w_prop(f1, weights1, L)
          p2 <- w_prop(f2, weights2, L)
          make_row(
            variable = v, type = "categorical", level = L,
            prop1 = p1, prop2 = p2,
            diff = ifelse(is.na(p1) | is.na(p2), NA_real_, p2 - p1),
            n1 = n1, mean1 = NA_real_, sd1 = NA_real_,
            n2 = n2, mean2 = NA_real_, sd2 = NA_real_,
            smd = NA_real_,
            miss_n1 = miss_n1, miss_pct1 = miss_pct1,
            miss_n2 = miss_n2, miss_pct2 = miss_pct2
          )
        })
      }
      do.call(rbind, rows)
    }
  })

  out <- do.call(rbind, out_list)
  row.names(out) <- NULL

  # rounding (only on numeric columns)
  num_cols <- vapply(out, is.numeric, logical(1))
  out[num_cols] <- lapply(out[num_cols], function(x) ifelse(is.na(x), NA, round(x, digits)))

  # column order
  desired <- c("variable","type","level",
               "prop1","prop2","diff",
               "n1","mean1","sd1",
               "n2","mean2","sd2",
               if (include_smd) "smd",
               "miss_n1","miss_pct1","miss_n2","miss_pct2")
  out <- out[, intersect(desired, names(out))]
  out
}
