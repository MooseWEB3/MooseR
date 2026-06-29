# MooseR

MooseR is a small R utility package for common data-cleaning and summary-table
tasks. It includes helpers for column-name cleaning, duplicate detection,
uniqueness checks, quote removal, one-row summaries for categorical and
continuous variables, covariate balance comparison, stable de-identified SID
generation, and date formatting.

The package is intentionally lightweight and has no external runtime package
dependencies. It requires R 4.1.0 or later.

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("MooseWEB3/MooseR", upgrade = "never")
```

Load the package:

```r
library(MooseR)
```

If you need to reinstall the latest GitHub version over an existing local copy:

```r
remotes::install_github("MooseWEB3/MooseR", force = TRUE, upgrade = "never")
```

## Functions

| Function | Purpose |
| --- | --- |
| `BD_fix_colname()` | Clean column names by replacing spaces and removing selected special characters. |
| `BD_get_duplicates()` | Find duplicated rows by one or more key columns. |
| `BD_check_unique()` | Check whether a column contains unique values. |
| `BD_count_unique_categories()` | Count unique categories in a variable. |
| `BD_quote_rm()` | Remove common quote characters from character and factor columns. |
| `BD_1_cat()` | Build a one-row count and percentage table for a categorical variable. |
| `BD_1_cont()` | Build a one-row summary table for a continuous variable. |
| `BD_compare_balance()` | Compare covariate balance between two data frames. |
| `BD_SID_creator()` | Create a stable de-identified SID from name, date of birth, and gender. |
| `BD_today()` | Return today's date in underscore and compact formats. |

## Examples

Clean column names:

```r
df <- data.frame("Col (1)" = 1:3, "Name's-Age" = c(20, 25, 30))
BD_fix_colname(df)
```

Summarize a categorical variable:

```r
df <- data.frame(group = c("A", "B", "A", NA, "C", "B", "B"))
BD_1_cat(df, "group", display_name = "Group")
```

Summarize a continuous variable:

```r
df <- data.frame(age = c(21, 34, 45, 52, NA, 63))
BD_1_cont(df, "age", display_name = "Age")
```

Find duplicate rows by key:

```r
BD_get_duplicates(mtcars, cyl, gear, .keep_all = FALSE)
```

Check uniqueness:

```r
BD_check_unique(iris, Species)
BD_check_unique(iris, "Species", na.rm = TRUE)
```

Create stable de-identified SIDs:

```r
BD_SID_creator(
  first_name = c("Ann", "Li"),
  last_name = c("O'Neil", "Xu"),
  DOB_ready = c("1990-07-03", "2000/01/02"),
  gender = c("F", "M")
)
```

Compare balance between two data frames:

```r
set.seed(1)
a <- data.frame(x = rnorm(100), g = sample(c("M", "F"), 100, TRUE))
b <- data.frame(x = rnorm(120, 0.2, 1.1), g = sample(c("M", "F"), 120, TRUE))

BD_compare_balance(a, b, vars = c("x", "g"), include_smd = TRUE)
```

Get today's date:

```r
BD_today()
```

## License

MIT license. Copyright (c) 2026 LisbonBulldog.
