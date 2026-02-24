library(h3jsr)
library(dplyr)
library(tidyr)
library(purrr)

# --- 1. DATA PROCESSING (Create Standalone Layer) ---
# This layer filters for species with >5% frequency in each grid
message("Processing grid_stats_SL...")

grid_stats_SL <- data_req %>%
  group_by(h3_id) %>%
  mutate(total_lists = n_distinct(GROUP.ID)) %>%
  ungroup() %>%
  group_by(h3_id, total_lists, COMMON.NAME) %>%
  summarise(detections = n_distinct(GROUP.ID), .groups = "drop") %>%
  mutate(frequency = detections / total_lists) %>%
  group_by(h3_id) %>%
  arrange(desc(frequency)) %>%
  filter(frequency >= 0.05) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  as.data.frame()

# --- 2. PREPARE LOOKUP ---
# We convert the long-format dataframe into a named list for the function.
# Result: A list where names are h3_ids and values are vectors of COMMON.NAME
species_lookup <- split(grid_stats_SL$COMMON.NAME, grid_stats_SL$h3_id)

# --- 3. DEFINE NEIGHBOR ANALYSIS FUNCTION ---
analyze_neighbors <- function(focal_id) {
  
  # A. Get Species in Focal Grid (x)
  focal_species <- species_lookup[[focal_id]]
  
  # Safety Check
  if (is.null(focal_species) || length(focal_species) == 0) {
    return(NULL)
  }
  
  n_focal <- length(focal_species)
  
  # B. Get Neighbors (Ring 1)
  # Using older 'get_ring' logic compatible with your version
  neighbors <- get_ring(focal_id, ring_size = 1, simple = TRUE)[[1]]
  
  # C. Iterate through neighbors
  map_dfr(neighbors, function(neighbor_id) {
    
    neighbor_species <- species_lookup[[neighbor_id]]
    
    # Handle missing neighbor data
    if (is.null(neighbor_species) || length(neighbor_species) == 0) {
      return(tibble(
        focal_grid = focal_id,
        neighbor_grid = neighbor_id,
        overlap_pct = NA,
        focal_count = n_focal,
        neighbor_count = 0,
        focal_species = paste(sort(focal_species), collapse = ", "),
        neighbor_species = NA
      ))
    }
    
    # D. CALCULATE OVERLAP
    common_species <- intersect(focal_species, neighbor_species)
    n_common <- length(common_species)
    
    # Percentage of Focal Grid's species found in Neighbor
    pct_overlap <- (n_common / n_focal) * 100
    
    # E. RETURN ROW
    tibble(
      focal_grid = focal_id,
      neighbor_grid = neighbor_id,
      overlap_pct = round(pct_overlap, 2),
      focal_count = n_focal,
      neighbor_count = length(neighbor_species),
      # Flatten lists to strings
      focal_species = paste(sort(focal_species), collapse = ", "),
      neighbor_species = paste(sort(neighbor_species), collapse = ", ")
    )
  })
}

# --- 4. RUN THE ANALYSIS ---
# Iterate over the unique IDs in your new dataframe
all_grids <- unique(grid_stats_SL$h3_id)

message("Analyzing neighbors for ", length(all_grids), " grids...")

neighbor_analysis_results <- map_dfr(all_grids, analyze_neighbors)

# --- 5. SAVE ---
print(head(neighbor_analysis_results))
write.csv(neighbor_analysis_results, "outputs/grid_neighbor_overlap.csv", row.names = FALSE)
message("Saved to outputs/grid_neighbor_overlap.csv")