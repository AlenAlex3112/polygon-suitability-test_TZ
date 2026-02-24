library(elevatr) 
library(terra)    
library(geodata)
library(rasterdiv)
library(sf)
library(h3jsr)

# --- 1. ELEVATION ---
message("Fetching elevation layer from AWS...")
elevation_raster <- get_elev_raster(map, z = 9, clip = "bbox")
r_elev <- rast(elevation_raster) 
r_elev <- mask(crop(r_elev, vect(map)), vect(map))
names(r_elev) <- "elevation"

# --- 2. RAINFALL ---
message("Fetching rainfall data...")
precip_stack <- worldclim_country("Tanzania", var = "prec", path = tempdir())
r_precip <- crop(precip_stack, vect(map), mask = TRUE) %>% sum() 
r_precip <- project(r_precip, r_elev)
names(r_precip) <- "rainfall_mm"

# --- 3. VEGETATION (Tree Cover) ---
global_tree_file <- "data/global_tree_cover_30s.tif"
if (!file.exists(global_tree_file)) {
  message("Global tree cover file not found. Downloading...")
  file_url <- "https://geodata.ucdavis.edu/geodata/landuse/WorldCover_trees_30s.tif"
  download.file(file_url, destfile = global_tree_file, mode = "wb")
}
r_global_ndvi <- rast(global_tree_file)
map_vect <- vect(map)
r_ndvi <- crop(r_global_ndvi, project(map_vect, crs(r_global_ndvi)), mask = TRUE)
r_ndvi <- project(r_ndvi, r_elev, method = "bilinear") 
names(r_ndvi) <- "tree_cover"
rm(r_global_ndvi); gc()

# --- 4. PREPARE H3 POLYGONS ---
h3_env_data <- cell_to_polygon(unique(grid_stats$h3_id), simple = FALSE) %>%
  rename(h3_id = h3_address)

# --- 5. SAVE (CRITICAL FIX) ---
# We must WRAP terra objects before saving to .RData
r_elev_packed   <- wrap(r_elev)
r_precip_packed <- wrap(r_precip)
r_ndvi_packed   <- wrap(r_ndvi)

save(h3_env_data, r_elev_packed, r_precip_packed, r_ndvi_packed, 
     file = "data/environmental_metrics.RData")

message("Processing complete. Rasters packed and saved.")