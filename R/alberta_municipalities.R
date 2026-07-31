# Alberta Municipal Affairs, 2024 Municipal Codes, updated June 3, 2024:
# https://open.alberta.ca/dataset/7b81986c-b05a-4b72-8f12-aec3a22970ae
#
# The 334 municipalities comprise 19 cities, 6 specialized municipalities,
# 63 municipal districts, 105 towns, 80 villages, 51 summer villages,
# 7 improvement districts, and 3 Special Areas. The Special Areas Board is an
# administrative body and is not included in this municipality registry.
alberta_official_municipalities <- local({
  municipalities <- c(
    # Cities
    "Airdrie", "Beaumont", "Brooks",
    "Calgary", "Camrose", "Chestermere",
    "Cold Lake", "Edmonton", "Fort Saskatchewan",
    "Grande Prairie", "Lacombe", "Leduc",
    "Lethbridge", "Lloydminster", "Medicine Hat",
    "Red Deer", "Spruce Grove", "St. Albert",
    "Wetaskiwin",
    # Specialized municipalities
    "Crowsnest Pass", "Jasper", "Lac La Biche County",
    "Mackenzie County", "Strathcona County", "Wood Buffalo",
    # Municipal districts
    "Acadia No. 34, M.D. of", "Athabasca County", "Barrhead No. 11, County of",
    "Beaver County", "Big Lakes County", "Bighorn No. 8, M.D. of",
    "Birch Hills County", "Bonnyville No. 87, M.D. of", "Brazeau County",
    "Camrose County", "Cardston County", "Clear Hills County",
    "Clearwater County", "Cypress County", "Fairview No. 136, M.D. of",
    "Flagstaff County", "Foothills County", "Forty Mile No. 8, County of",
    "Grande Prairie No. 1, County of", "Greenview No. 16, M.D. of", "Kneehill County",
    "Lac Ste. Anne County", "Lacombe County", "Lamont County",
    "Leduc County", "Lesser Slave River No. 124, M.D. of", "Lethbridge County",
    "Minburn No. 27, County of", "Mountain View County", "Newell, County of",
    "Northern Lights, County of", "Northern Sunrise County", "Opportunity No. 17, M.D. of",
    "Paintearth No. 18, County of", "Parkland County", "Peace No. 135, M.D. of",
    "Pincher Creek No. 9, M.D. of", "Ponoka County", "Provost No. 52, M.D. of",
    "Ranchland No. 66, M.D. of", "Red Deer County", "Rocky View County",
    "Saddle Hills County", "Smoky Lake County", "Smoky River No. 130, M.D. of",
    "Spirit River No. 133, M.D. of", "St. Paul No. 19, County of", "Starland County",
    "Stettler No. 6, County of", "Sturgeon County", "Taber, M.D. of",
    "Thorhild County", "Two Hills No. 21, County of", "Vermilion River, County of",
    "Vulcan County", "Wainwright No. 61, M.D. of", "Warner No. 5, County of",
    "Westlock County", "Wetaskiwin No. 10, County of", "Wheatland County",
    "Willow Creek No. 26, M.D. of", "Woodlands County", "Yellowhead County",
    # Towns
    "Athabasca", "Banff", "Barrhead",
    "Bashaw", "Bassano", "Beaverlodge",
    "Bentley", "Blackfalds", "Bon Accord",
    "Bonnyville", "Bow Island", "Bowden",
    "Bruderheim", "Calmar", "Canmore",
    "Cardston", "Carstairs", "Castor",
    "Claresholm", "Coaldale", "Coalhurst",
    "Cochrane", "Coronation", "Crossfield",
    "Daysland", "Devon", "Diamond Valley",
    "Didsbury", "Drayton Valley", "Drumheller",
    "Eckville", "Edson", "Elk Point",
    "Fairview", "Falher", "Fort Macleod",
    "Fox Creek", "Gibbons", "Grimshaw",
    "Hanna", "Hardisty", "High Level",
    "High Prairie", "High River", "Hinton",
    "Innisfail", "Irricana", "Killam",
    "Lamont", "Legal", "Magrath",
    "Manning", "Mayerthorpe", "McLennan",
    "Milk River", "Millet", "Morinville",
    "Mundare", "Nanton", "Nobleford",
    "Okotoks", "Olds", "Onoway",
    "Oyen", "Peace River", "Penhold",
    "Picture Butte", "Pincher Creek", "Ponoka",
    "Provost", "Rainbow Lake", "Raymond",
    "Redcliff", "Redwater", "Rimbey",
    "Rocky Mountain House", "Sedgewick", "Sexsmith",
    "Slave Lake", "Smoky Lake", "Spirit River",
    "St. Paul", "Stavely", "Stettler",
    "Stony Plain", "Strathmore", "Sundre",
    "Swan Hills", "Sylvan Lake", "Taber",
    "Thorsby", "Three Hills", "Tofield",
    "Trochu", "Two Hills", "Valleyview",
    "Vauxhall", "Vegreville", "Vermilion",
    "Viking", "Vulcan", "Wainwright",
    "Wembley", "Westlock", "Whitecourt",
    # Villages
    "Acme", "Alberta Beach", "Alix",
    "Alliance", "Amisk", "Andrew",
    "Arrowwood", "Barnwell", "Barons",
    "Bawlf", "Beiseker", "Berwyn",
    "Big Valley", "Bittern Lake", "Boyle",
    "Breton", "Carbon", "Carmangay",
    "Caroline", "Champion", "Chauvin",
    "Chipman", "Clive", "Clyde",
    "Consort", "Coutts", "Cowley",
    "Cremona", "Czar", "Delburne",
    "Delia", "Donalda", "Donnelly",
    "Duchess", "Edberg", "Edgerton",
    "Elnora", "Empress", "Foremost",
    "Forestburg", "Girouxville", "Glendon",
    "Glenwood", "Halkirk", "Hay Lakes",
    "Heisler", "Hill Spring", "Hines Creek",
    "Holden", "Hughenden", "Hussar",
    "Innisfree", "Irma", "Kitscoty",
    "Linden", "Lomond", "Longview",
    "Lougheed", "Mannville", "Marwayne",
    "Milo", "Morrin", "Munson",
    "Myrnam", "Nampa", "Paradise Valley",
    "Rockyford", "Rosalind", "Rosemary",
    "Rycroft", "Ryley", "Spring Lake",
    "Standard", "Stirling", "Veteran",
    "Vilna", "Warburg", "Warner",
    "Waskatenau", "Youngstown",
    # Summer villages
    "Argentia Beach", "Betula Beach", "Birch Cove",
    "Birchcliff", "Bondiss", "Bonnyville Beach",
    "Burnstick Lake", "Castle Island", "Crystal Springs",
    "Ghost Lake", "Golden Days", "Grandview",
    "Gull Lake", "Half Moon Bay", "Horseshoe Bay",
    "Island Lake", "Island Lake South", "Itaska Beach",
    "Jarvis Bay", "Kapasiwin", "Lakeview",
    "Larkspur", "Ma-Me-O Beach", "Mewatha Beach",
    "Nakamun Park", "Norglenwold", "Norris Beach",
    "Parkland Beach", "Pelican Narrows", "Point Alison",
    "Poplar Bay", "Rochon Sands", "Ross Haven",
    "Sandy Beach", "Seba Beach", "Silver Beach",
    "Silver Sands", "South Baptiste", "South View",
    "Sunbreaker Cove", "Sundance Beach", "Sunrise Beach",
    "Sunset Beach", "Sunset Point", "Val Quentin",
    "Waiparous", "West Baptiste", "West Cove",
    "Whispering Hills", "White Sands", "Yellowstone",
    # Improvement districts
    "Improvement District No. 04 (Waterton)",
    "Improvement District No. 09 (Banff)",
    "Improvement District No. 12 (Jasper National Park)",
    "Improvement District No. 13 (Elk Island)",
    "Improvement District No. 24 (Wood Buffalo)",
    "Improvement District No. 25 (Willmore Wilderness)",
    "Kananaskis Improvement District",
    # Special Areas
    "Special Areas No. 2", "Special Areas No. 3", "Special Areas No. 4"
  )

  stopifnot(length(municipalities) == 334L, !anyDuplicated(municipalities))

  function() municipalities
})

normalize_alberta_municipality <- function(x) {
  x <- tolower(x)
  x <- gsub("\\b0+([0-9]+)\\b", "\\1", x, perl = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x, perl = TRUE)
  x <- trimws(gsub("\\s+", " ", x, perl = TRUE))
  sub(
    "^(?:city|town|village|summer village|municipality) of ",
    "",
    x,
    perl = TRUE
  )
}

alberta_municipality_aliases <- local({
  aliases <- alberta_official_municipalities()
  common_order <- aliases
  common_order <- sub(
    "^(.+), M\\.D\\. of$",
    "Municipal District of \\1",
    common_order
  )
  common_order <- sub(
    "^(.+), County of$",
    "County of \\1",
    common_order
  )
  core <- aliases
  core <- sub(" No\\. [0-9]+, M\\.D\\. of$", "", core)
  core <- sub(" No\\. [0-9]+, County of$", "", core)
  core <- sub(", M\\.D\\. of$", "", core)
  core <- sub(", County of$", "", core)
  aliases <- unique(c(aliases, common_order, core))

  function() aliases
})

is_alberta_municipality_name <- local({
  normalized <- unique(normalize_alberta_municipality(
    alberta_municipality_aliases()
  ))

  function(x) normalize_alberta_municipality(x) %in% normalized
})

alberta_municipality_candidate_fragments <- local({
  aliases <- unique(normalize_alberta_municipality(
    alberta_municipality_aliases()
  ))
  words <- strsplit(aliases, " ", fixed = TRUE)
  fragments <- unlist(
    lapply(
      words,
      function(value) {
        max_size <- min(3L, length(value))

        unlist(
          lapply(
            seq_len(max_size),
            function(size) {
              starts <- seq_len(length(value) - size + 1L)
              vapply(
                starts,
                function(start) {
                  paste(value[seq.int(start, start + size - 1L)], collapse = " ")
                },
                character(1)
              )
            }
          ),
          use.names = FALSE
        )
      }
    ),
    use.names = FALSE
  )
  fragments <- unique(fragments)

  function() fragments
})

is_alberta_municipality_candidate <- local({
  fragments <- alberta_municipality_candidate_fragments()

  function(x) {
    x <- sub(
      "(?i)^(?:Mr|Mrs|Ms|Miss|Dr|Doctor|RN|Paramedic|EMT)\\.?\\s+",
      "",
      x,
      perl = TRUE
    )
    normalize_alberta_municipality(x) %in% fragments
  }
})

escape_name_regex_literal <- function(x) {
  special <- c("\\", ".", "^", "$", "|", "(", ")", "[", "]", "{", "}", "*", "+", "?")

  vapply(
    strsplit(x, "", fixed = TRUE),
    function(characters) {
      paste0(
        ifelse(characters %in% special, paste0("\\", characters), characters),
        collapse = ""
      )
    },
    character(1)
  )
}

alberta_municipality_regex_pattern <- local({
  cache <- list()

  function(multiword_only = FALSE) {
    key <- if (isTRUE(multiword_only)) "multiword" else "all"

    if (!is.null(cache[[key]])) {
      return(cache[[key]])
    }

    aliases <- alberta_municipality_aliases()

    if (isTRUE(multiword_only)) {
      aliases <- aliases[grepl("\\s", aliases, perl = TRUE)]
    }

    aliases <- aliases[order(nchar(aliases), decreasing = TRUE)]
    escaped <- escape_name_regex_literal(aliases)
    escaped <- gsub(" ", "\\s+", escaped, fixed = TRUE)
    cache[[key]] <- paste0(
      "(?<![A-Za-z'-])(?i:(?:",
      paste(escaped, collapse = "|"),
      "))(?![A-Za-z'-])"
    )
    cache[[key]]
  }
})

alberta_municipality_exact_guard <- function() {
  paste0(
    "(?!",
    alberta_municipality_regex_pattern(),
    "(?=\\s*(?:$|[,.;:!?])))"
  )
}

remove_alberta_municipality_phrases <- function(text) {
  pattern <- alberta_municipality_regex_pattern(multiword_only = TRUE)
  has_municipality <- grepl(pattern, text, perl = TRUE)

  if (any(has_municipality)) {
    text[has_municipality] <- gsub(
      pattern,
      " ",
      text[has_municipality],
      perl = TRUE
    )
  }

  text
}
