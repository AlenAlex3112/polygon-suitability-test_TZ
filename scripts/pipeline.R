library(skimmr)
library(sf)
library(h3jsr)
library(dplyr)
library(purrr)
library(tidyverse)
library(glue)

map <- st_read("data/map.geojson", quiet = TRUE) %>%
    st_transform(4326)

h3_ids_master <- map %>%
    polygon_to_cells(res = 7, simple = FALSE) %>%
    unnest(h3_addresses) %>%
    pull(h3_addresses) %>%
    unique()

if (!file.exists("data/clean_data.RData")) {
    message("Cached data not found. Running preparation script...")
    source("scripts/ebd_prep.R", local = TRUE)
  } else {
    message("Loading cached data...")
    load("data/clean_data.RData") 
  }
  
  source("scripts/data_prep.R", local = TRUE)
  source("scripts/function.R", local = TRUE)
  source("scripts/pre-leaflet.R", local = TRUE)
  source("scripts/geo-data.R", local = TRUE)
  source("scripts/leaflet.R", local = TRUE)
  
