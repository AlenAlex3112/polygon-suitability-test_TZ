# scripts/ebd_prep.R

# --- 1. LOAD UNVERIFIED DATA ----------------------------------------
# Note: Ensure this path is correct relative to your working directory
data_req = read.delim("data/ebd_TZ_relJan-2026.txt", sep = "\t", 
                  header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

# Clean unverified data
cols_to_remove <- c("GLOBAL.UNIQUE.IDENTIFIER", "TIME.OBSERVATIONS.STARTED", 
                    "LAST.EDITED.DATE", "TAXON.CONCEPT.ID", "SUBSPECIES.COMMON.NAME", 
                    "BEHAVIOR.CODE", "COUNTRY", "COUNTRY.CODE", "IBA.CODE", 
                    "BCR.CODE", "USFWS.CODE", "ATLAS.BLOCK", "OBSERVER.ORCID.ID", 
                    "OBSERVATION.TYPE", "PROJECT.NAMES", "PROJECT.IDENTIFIERS", 
                    "EFFORT.AREA.HA", "REASON", "X", "YEAR", "MONTH", "DAY.M", 
                    "M.YEAR", "M.MONTH")

data_req <- data_req %>%
  dplyr::select(-any_of(cols_to_remove)) %>%
  filter(CATEGORY %in% c("species", "issf")) %>%
  mutate(GROUP.ID = if_else(!is.na(GROUP.IDENTIFIER), GROUP.IDENTIFIER, SAMPLING.EVENT.IDENTIFIER)) |> 
  dplyr::select(COMMON.NAME, LATITUDE, LONGITUDE, GROUP.ID, APPROVED)

# Calculate H3 Cells
data_req <- data_req %>%
  mutate(h3_id = point_to_cell(
    pick(LONGITUDE, LATITUDE),
    res = 7
  ))

# --- 4. GEOSPATIAL FILTERING ----------------------------------------
# Filter points strictly inside the state polygon
unique_lists <- data_req %>%
  distinct(GROUP.ID, LATITUDE, LONGITUDE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# 'map' object is inherited from the main loop environment
lists_within_polygon <- st_join(unique_lists, map %>% dplyr::select(id), join = st_intersects) %>%
  st_drop_geometry() %>%
  dplyr::select(GROUP.ID, polygon_id = id)

data_req <- data_req %>%
  left_join(lists_within_polygon, by = "GROUP.ID") %>%
  filter(!is.na(polygon_id))

rm(unique_lists, lists_within_polygon)

# --- 5. SAVE --------------------------------------------------------
save(data_req, file = "data/clean_data.RData")
