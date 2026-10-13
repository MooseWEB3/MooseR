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

#' Alberta hospitals: names, addresses and coordinates
#'
#' An offline directory of 112 Alberta hospital and specialist inpatient
#' facilities, retrieved from Alberta Health Services (AHS) on October 12, 2026.
#' Names, addresses and coordinates are taken directly from AHS location pages.
#'
#' @format A data frame with 112 rows and 14 columns:
#' \describe{
#'   \item{facility_id}{Character. Unique AHS facility identifier.}
#'   \item{hospital_name}{Character. Official facility name in the source.}
#'   \item{address}{Character. Full street address, city, province and postal code.}
#'   \item{street_address}{Character. Street address supplied by AHS.}
#'   \item{city}{Character. Municipality or community supplied by AHS.}
#'   \item{province}{Character. Always `Alberta`.}
#'   \item{postal_code}{Character. Canadian postal code, including its space.}
#'   \item{latitude}{Numeric. AHS web-map latitude in decimal degrees.}
#'   \item{longitude}{Numeric. AHS web-map longitude in decimal degrees;
#'     negative values are west of Greenwich.}
#'   \item{inclusion_basis}{Character. `ahs_hospitals_service` for 104 Alberta
#'     locations in the Hospitals service directory, or `specialist_supplement`
#'     for eight additional inpatient specialist facilities.}
#'   \item{inclusion_source_url}{Character. Official source supporting inclusion.}
#'   \item{source_url}{Character. AHS page from which the location fields were read.}
#'   \item{retrieved_on}{Date. Location retrieval date in the America/Edmonton
#'     time zone, not the date AHS last updated the record.}
#'   \item{emergency_24h}{Integer flag: `1L` for a general or pediatric emergency
#'     department with a published regular 24-hour, seven-day schedule; `0L`
#'     for no emergency department or a department with limited scheduled hours.
#'     Reviewed October 12, 2026. Temporary closures do not change the flag.}
#' }
#'
#' @details
#' Includes all Alberta locations in the AHS Hospitals service directory,
#' including health centres listed there, and eight additional inpatient
#' specialist facilities: Alberta Hospital Edmonton, Centennial Centre for Mental
#' Health and Brain Injury, Claresholm Centre for Mental Health & Addictions,
#' Southern Alberta Forensic Psychiatry Centre, Villa Caritas, Lois Hole Hospital
#' for Women, Mazankowski Alberta Heart Institute and Cross Cancer Institute.
#' Separately named facilities on the same campus remain separate records.
#'
#' Lloydminster Hospital is excluded because its address is in Saskatchewan.
#' Standalone outpatient clinics, continuing-care/auxiliary facilities and
#' historical hospitals are outside the scope. Inclusion does not establish
#' current emergency service availability or inpatient capacity. This is a
#' dated directory snapshot, not a guaranteed exhaustive legal hospital register.
#'
#' The emergency flag uses the hospital's own AHS Emergency Services schedule,
#' not general hospital opening hours, urgent care, psychiatric consultation or
#' maternity triage. An emergency department at a neighbouring or co-located
#' hospital is not inherited. Published ongoing reduced weekly schedules count
#' as `0L`; isolated temporary disruptions do not change a scheduled 24-hour
#' department to `0L`. This is a regular-schedule flag, not live availability.
#' The snapshot has 94 values of `1L` and 18 values of `0L` (seven limited-hours
#' departments and 11 facilities without their own emergency department).
#'
#' Per-facility ED evidence, assessment and review date are installed in
#' `system.file("extdata", "alberta-hospitals-emergency.csv", package = "MooseR")`.
#' ED hours are reviewed separately from location updates; ambiguous or missing
#' evidence requires review rather than being silently assigned `0L`.
#'
#' Coordinates are copied from the AHS web map, without geocoding or rounding,
#' and used as WGS84-compatible decimal degrees. The pages do not specify datum
#' or survey accuracy. These are facility map locations, not verified entrances.
#'
#' The source CSV is also installed at
#' `system.file("extdata", "alberta-hospitals.csv", package = "MooseR")`.
#' Package maintainers can refresh it with
#' `data-raw/update-alberta-hospitals.py` and rebuild the R data file offline with
#' `data-raw/create-alberta-hospitals.R`. No network request or API key is needed
#' to use the bundled data. The existing [alberta_health_facilities] name list
#' has a broader scope and remains a separate dataset.
#'
#' @source Alberta Health Services, Find Healthcare, retrieved October 12, 2026:
#'   \url{https://www.albertahealthservices.ca/findhealth/service.aspx?Id=1001416}.
#'   Additional inclusion sources are recorded per row. AHS source website terms:
#'   \url{https://www.albertahealthservices.ca/about/Page635.aspx}.
#'   This independent compilation is not endorsed by AHS; the MooseR code
#'   licence does not relicense the source website.
#'
#' @examples
#' data("alberta_hospitals", package = "MooseR")
#' head(alberta_hospitals[c("hospital_name", "address", "latitude", "longitude")])
#' subset(alberta_hospitals, city == "Edmonton")
#' subset(alberta_hospitals, emergency_24h == 1L)
#'
"alberta_hospitals"
