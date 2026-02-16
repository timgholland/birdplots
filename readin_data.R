### file to read in data to be accessed by all pages

library(tidyverse)

# ---- palette + helpers ----
ebPal <- c("#e76f51","#f4a261","#e9c46a","#2a9d8f","#264653")

muteCol <- function(col, sVal, vVal){
  hsv(h = rgb2hsv(t(coords(hex2RGB(col))))[1], s = sVal, v = vVal)
}

# ---- paths ----
clements_path <- "C:/Users/Tim.Holland/Dropbox/Other/birdplots2026/eBird-Clements-v2025.csv"
myebird_path  <- "C:/Users/Tim.Holland/Dropbox/Other/birdplots2026/MyEBirdData.csv"

# ---- lookup tables ----
orders <- readr::read_csv("order_names.csv", show_col_types = FALSE)

# ISO country codes used to derive ABA/non-ABA (CAN/USA/SPM)
iso <- readr::read_csv(
  file = "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv",
  show_col_types = FALSE
) %>%
  dplyr::rename(iso2 = `alpha-2`, iso3 = `alpha-3`, country = name) %>%
  dplyr::select(country, iso2, iso3)

# ---- Clements / eBird integrated checklist (v2025) ----
# Column names differ by version; standardization here prevents downstream breakage.
clem_raw <- readr::read_csv(clements_path, show_col_types = FALSE)

# Standardize names without adding new dependencies
names(clem_raw) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(clem_raw)))
names(clem_raw) <- gsub("_+$", "", names(clem_raw))

clem <- clem_raw %>%
  dplyr::rename(
    taxon_sort        = sort_v2025,
    common_name       = english_name,
    latin_name_incSubsp = scientific_name
  ) %>%
  # extinct is 1/0 in v2025; retain rows where extinct is missing or 0
  dplyr::filter(is.na(extinct) | extinct == 0) %>%
  # retain core fields used in plots/joins; keep range if later used
  dplyr::select(
    taxon_sort,
    species_code,
    taxon_concept_id,
    category,
    common_name,
    latin_name_incSubsp,
    order,
    family,
    range,
    extinct,
    extinct_year,
    sort_v2024
  ) %>%
  tidyr::separate(
    latin_name_incSubsp,
    sep = " ",
    into = c("genus","species","subspA","subspB"),
    remove = FALSE,
    extra = "merge",
    fill = "right"
  ) %>%
  tidyr::unite(latin_binomial, c(genus, species), sep = " ", remove = FALSE) %>%
  tidyr::unite(subspTemp, c(subspA, subspB), sep = " ", remove = TRUE) %>%
  dplyr::mutate(
    subspecies = gsub("(^NA\\s*|\\s*NA$)", "", subspTemp),
    subspecies = dplyr::na_if(trimws(subspecies), "")
  ) %>%
  dplyr::left_join(
    dplyr::select(orders, order, order_with_desc),
    by = "order"
  ) %>%
  dplyr::rename(family_with_desc = family) %>%
  tidyr::separate(
    family_with_desc,
    sep = " ",
    into = c("family","famTemp"),
    remove = FALSE,
    extra = "merge",
    fill = "right"
  ) %>%
  dplyr::select(-subspTemp, -famTemp)

clem.sp <- dplyr::filter(clem, category == "species")

# Clean-up steps consistent with prior logic, but compatible with v2025 structure
clem <- clem %>%
  dplyr::mutate(
    genus = dplyr::if_else(genus %in% clem.sp$genus, genus, NA_character_),
    latin_binomial = dplyr::if_else(category %in% c("spuh"), NA_character_, latin_binomial),
    subspecies = dplyr::if_else(subspecies == " ", NA_character_, subspecies),
    species = dplyr::if_else(species == "sp.", NA_character_, species)
  )

# ---- California state list (optional; legacy input) ----
# Reading as lines prevents missing/blank column-name issues during filtering.
# This block can be removed once a region-species-list approach is adopted.
calist_path <- "CA_main_list.txt"
if (file.exists(calist_path)) {
  calist <- readr::read_lines(calist_path) %>%
    tibble::tibble(list = .) %>%
    dplyr::filter(stringr::str_detect(list, "\t\t")) %>%
    dplyr::mutate(list = stringr::str_replace(list, "\t\t", ""))
  
  calist <- stringr::str_split(calist$list, pattern = "\\(")
  calist <- unlist(calist)[seq(from = 2, to = length(unlist(calist)), by = 2)]
  calist <- stringr::str_split(calist, pattern = "\\)")
  calist <- unlist(calist)[seq(from = 1, to = length(unlist(calist)) - 1, by = 2)]
  calist <- gsub("Porphyrio martinicus","Porphyrio martinica", calist)
  
  calist <- tibble::tibble(latin_binomial = calist) %>%
    dplyr::left_join(clem, by = c("latin_binomial" = "latin_binomial"))
} else {
  calist <- tibble::tibble()
}

# ---- My eBird Data export ----
# Column positions can vary across exports; selecting by names avoids brittle indexing.
myeb <- data.table::fread(
  myebird_path,
  encoding = "UTF-8",
  fill = TRUE,
  data.table = FALSE
) %>%
  tibble::as_tibble() %>%
  dplyr::filter(Count != 0) %>%
  dplyr::transmute(
    common_name_incSubsp = `Common Name`,
    latin_name_incSubsp  = `Scientific Name`,
    count                = Count,
    taxon_sort           = `Taxonomic Order`,
    state_prov_code      = `State/Province`,
    county               = County,
    location             = Location,
    latitude             = Latitude,
    longitude            = Longitude,
    date                 = as.Date(Date),
    time                 = Time
  ) %>%
  dplyr::mutate(iso2 = substr(state_prov_code, 1, 2)) %>%
  dplyr::left_join(iso, by = "iso2") %>%
  # Join to Clements using scientific name; species_code is carried in from clem for stability
  dplyr::left_join(dplyr::select(clem, -common_name), by = "latin_name_incSubsp") %>%
  dplyr::mutate(
    state_prov_2 = substr(state_prov_code, 4, 5),
    aba = dplyr::if_else(iso3 %in% c("CAN","USA","SPM"), 1L, 0L),
    year = as.integer(format(date, "%Y"))
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    month_year = format(date, format = "%b %Y"),
    month_year = factor(month_year, levels = unique(month_year))
  )

# Species-level view used in plots; retains prior inclusion logic
myeb.sp <- dplyr::filter(
  myeb,
  category %in% c("species","form","group (monotypic)","group (polytypic)") |
    latin_binomial == "Columba livia"
)