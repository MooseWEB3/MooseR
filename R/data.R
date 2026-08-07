#' Official Alberta municipalities
#'
#' A named list containing the 334 official Alberta municipalities recorded in
#' the Alberta Municipal Affairs 2024 Municipal Codes, updated June 3, 2024.
#' The administrative body `Special Areas Board` is not included. Communities
#' without official municipality status, such as Fort McMurray, are also not
#' part of this dataset.
#'
#' @format A named list with eight character vectors:
#' \describe{
#'   \item{cities}{19 cities.}
#'   \item{specialized_municipalities}{6 specialized municipalities.}
#'   \item{municipal_districts}{63 municipal districts and counties.}
#'   \item{towns}{105 towns.}
#'   \item{villages}{80 villages.}
#'   \item{summer_villages}{51 summer villages.}
#'   \item{improvement_districts}{7 improvement districts.}
#'   \item{special_areas}{3 Special Areas.}
#' }
#'
#' @source Alberta Municipal Affairs, Municipal Codes (June 3, 2024),
#'   \url{https://open.alberta.ca/dataset/7b81986c-b05a-4b72-8f12-aec3a22970ae}
#'
#' @examples
#' names(alberta_municipalities)
#' alberta_municipalities$cities
#' unlist(alberta_municipalities, use.names = FALSE)
#'
"alberta_municipalities"

#' Alberta Health Services facility names
#'
#' A named list containing 1,150 unique facility names from the Alberta Health
#' Services Find Healthcare facility-name search as of July 31, 2026. Because
#' the source provides names without a reliable facility-type field, the list
#' is grouped alphabetically by the first character of each facility name.
#'
#' @format A named list of character vectors. Elements are named `0-9` and
#'   `A` through `Z` where facilities with that initial are present. Across all
#'   elements, the list contains 1,150 unique facility names.
#'
#' @source Alberta Health Services, Find Healthcare,
#'   \url{https://www.albertahealthservices.ca/findhealth/search.aspx}
#'
#' @examples
#' names(alberta_health_facilities)
#' head(alberta_health_facilities$A)
#' unlist(alberta_health_facilities, use.names = FALSE)
#'
"alberta_health_facilities"
