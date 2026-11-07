# MooseR 0.0.0.9047

## R 4.6.1 upgrade

* Tested with R 4.6.1: `R CMD check --as-cran` passes with no errors, warnings
  or notes. All base-R functions and arguments the package and its tests use
  already exist in R 4.3.0. `DESCRIPTION` still declares R (>= 4.1.0).
* Accent removal no longer depends on the platform's `iconv()`
  transliteration, which gave different results on macOS (for example
  `"'e"` for `"é"`). A built-in Latin-1 and Latin Extended-A table now gives
  the same result on every computer. This affects
  `BD_SID_creator(strip_accents = TRUE)` and the school, drug and known-name
  matching used by personal-name detection and masking.
* Documentation regenerated with roxygen2 8.1.1. Help text that existed only
  in `man/` (examples for the startup-package helpers, the `explain = TRUE`
  columns of `detect_person_names()`) is now kept in the source.
* `stats`, `tools` and `utils` are declared in `Imports`.
* The 20 `Moose_`-prefixed aliases keep their exports when documentation is
  regenerated.
* Three comment-only legacy files were removed.

# MooseR 0.0.0.9046

* `Moose_nearest_hospitals_2()`: the same 13 output columns as
  `Moose_nearest_hospitals()`, built on the sf and osrm packages and the
  public osrm server; accepts sf input.
* `Moose_nearest_hospitals(engine = "google")` can call the Google Routes API
  directly with your own `GOOGLE_MAPS_API_KEY` (`google_backend = "google"`),
  for networks that block the MooseR proxy.
