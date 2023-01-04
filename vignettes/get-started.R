## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  message = FALSE,
  fig.dpi = 300,
  fig.align = "center",
  fig.width = 5,
  fig.height = 5
)
# options(tigris_use_cache = TRUE)

## ----results="hide"-----------------------------------------------------------
puma_sf <- get_acs(
  geography = "public use microdata area",
  variables = "B19013_001", # This can be any variables, since we won't be using this table as a control table.
  state = "IL",
  geometry = TRUE,
  year = 2018,
  keep_geo_vars = TRUE
) %>%
  select(PUMA = PUMACE10) %>%
  filter(PUMA == "03525")

tract_to_puma_correspondence <-
  read.csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt",
    colClasses = c("character")
  ) %>%
  rename(PUMA = "PUMA5CE") %>%
  filter(PUMA == puma_sf$PUMA) # only keep Census tracts in Chicago City (Central)

tract_sf <- get_acs(
  geography = "tract",
  variables = "B19013_001", # This can be any variables, since we won't be using this table as a control table.
  state = "IL",
  county = unique(tract_to_puma_correspondence$COUNTYFP),
  geometry = TRUE,
  year = 2018,
  # survey = "acs5",
  keep_geo_vars = TRUE
) %>%
  select(COUNTYFP, TRACTCE) %>%
  filter(TRACTCE %in% unique(tract_to_puma_correspondence$TRACTCE) &
    COUNTYFP %in% unique(tract_to_puma_correspondence$COUNTYFP))

## ----echo=FALSE---------------------------------------------------------------
mapview(list(puma_sf, tract_sf))

## ---- results='hide'----------------------------------------------------------
pums_dict_2018 <- pums_variables %>%
  filter(year == 2018, survey == "acs1") %>%
  distinct(var_code, var_label, data_type, level)

