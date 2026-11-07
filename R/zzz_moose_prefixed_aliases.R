# Moose-prefixed compatibility aliases.
# Existing function names are kept so older scripts continue to run.

#' Moose-prefixed compatibility aliases
#'
#' These functions provide `Moose_`-prefixed names for existing MooseR
#' functions while keeping the older names available for backward compatibility.
#'
#' For example, `Moose_fix_colname()` calls `BD_fix_colname()`,
#' and `Moose_mask_person_names()` calls `mask_person_names()`.
#'
#' @param ... Arguments passed to the original non-prefixed function.
#' @return The return value from the original non-prefixed function.
#' @name moose_prefixed_aliases
#' @examples
#' Moose_default_mooser_packages()
NULL

#' @rdname moose_prefixed_aliases
#' @export
Moose_1_cat <- function(...) {
  BD_1_cat(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_1_cont <- function(...) {
  BD_1_cont(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_SID_creator <- function(...) {
  BD_SID_creator(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_check_unique <- function(...) {
  BD_check_unique(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_compare_balance <- function(...) {
  BD_compare_balance(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_count_unique_categories <- function(...) {
  BD_count_unique_categories(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_fix_colname <- function(...) {
  BD_fix_colname(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_get_duplicates <- function(...) {
  BD_get_duplicates(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_quote_rm <- function(...) {
  BD_quote_rm(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_today <- function(...) {
  BD_today(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_apply_name_masking_rules <- function(...) {
  apply_name_masking_rules(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_check_name_masking <- function(...) {
  check_name_masking(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_default_mooser_packages <- function(...) {
  default_mooser_packages(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_detect_person_names <- function(...) {
  detect_person_names(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_disable_mooser_startup_packages <- function(...) {
  disable_mooser_startup_packages(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_enable_mooser_startup_packages <- function(...) {
  enable_mooser_startup_packages(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_load_mooser_packages <- function(...) {
  load_mooser_packages(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_load_packages <- function(...) {
  load_packages(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_mask_person_names <- function(...) {
  mask_person_names(...)
}

#' @rdname moose_prefixed_aliases
#' @export
Moose_setup_name_masking <- function(...) {
  setup_name_masking(...)
}
