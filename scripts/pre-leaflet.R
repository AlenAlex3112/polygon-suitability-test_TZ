h3_polygons <- cell_to_polygon(unique(grid_stats$h3_id), simple = FALSE) %>%
  rename(h3_id = h3_address)

word_safe_wrap <- function(x, width = 50) {
  if (is.na(x) || x == "None" || x == "") return("None")
  paste(strwrap(x, width = width), collapse = "<br>")
}

unified_stats <- grid_counts %>%
  left_join(grid_effort, by = "h3_id") %>%
  # Join Environmental Data (drop geometry to avoid conflict)
  left_join(st_drop_geometry(h3_polygons), by = "h3_id") %>% 
  left_join(results %>% dplyr::select(h3_id, polygon_id, outlier_count, outlier_list), by = "h3_id") %>%
  left_join(polygon_lookup, by = "polygon_id") %>%
  mutate(
    subregion_name = ifelse(is.na(subregion), "Unknown", subregion),
    wrapped_rarities = sapply(outlier_list, word_safe_wrap, width = 50),
    
    # Create the label
    hover_label = glue(
      "<b>{subregion_name}</b><br><hr style='margin:3px 0;'>",
      "<b>Species:</b> {unique_species}<br>",
      "<b>Checklists:</b> {checklist_count} ({round(effort_percentage, 1)}%)<br>",
      "<br>",
      "<p style='color: #d9534f; margin: 5px 0 0 0;'><b>Regional Rarities ({outlier_count}):</b></p>",
      "<i style='font-size: 0.85em; color: #555;'>{wrapped_rarities}</i>"
    ) %>% map(htmltools::HTML)
  )

h3_polygons_map <- h3_polygons %>%
  left_join(unified_stats %>% dplyr::select(h3_id, hover_label), by = "h3_id")

# 1. Recreate map_corr (Rarity Layer)
# Uses 'results' to identify which cells have rarity data
map_corr <- cell_to_polygon(unique(results$h3_id), simple = FALSE) %>%
  rename(h3_id = h3_address) %>%
  left_join(unified_stats, by = "h3_id") # Attaches counts, labels, etc.

# 2. Recreate map_density_h3 (Species Count Layer)
# Uses 'grid_counts' to identify cells with species data
map_density_h3 <- cell_to_polygon(unique(grid_counts$h3_id), simple = FALSE) %>%
  rename(h3_id = h3_address) %>%
  left_join(unified_stats, by = "h3_id")

# 3. Recreate map_effort_h3 (Checklist Count Layer)
# Uses 'grid_effort' to identify cells with effort data
map_effort_h3 <- cell_to_polygon(unique(grid_effort$h3_id), simple = FALSE) %>%
  rename(h3_id = h3_address) %>%
  left_join(unified_stats, by = "h3_id")

# 4. Recreate empty_grids_h3 (No Data Zones)
# Note: Ensure h3_ids_master is defined. If not, we regenerate it from the map.
if(!exists("h3_ids_master")) {
  h3_ids_master <- get_ust_grid(map, res = 7)
}

empty_grids_h3 <- cell_to_polygon(
  setdiff(h3_ids_master, unique(grid_counts$h3_id)), 
  simple = FALSE
)

# 1. ATTACH LABELS TO ENVIRONMENTAL DATA
# This takes the label you made in bird data and adds it to the elevation/rain map
h3_polygons <- cell_to_polygon(unique(grid_stats$h3_id), simple = FALSE) %>%
  rename(h3_id = h3_address)

h3_polygons <- h3_polygons %>%
  left_join(unified_stats %>% dplyr::select(h3_id, hover_label), by = "h3_id")

# 2. ATTACH LABELS TO OTHER LAYERS (Just to be safe)
map_corr <- map_corr %>%
  dplyr::select(-any_of("hover_label")) %>% # Remove old label if exists to avoid duplicates
  left_join(unified_stats %>% dplyr::select(h3_id, hover_label), by = "h3_id")

map_density_h3 <- map_density_h3 %>%
  dplyr::select(-any_of("hover_label")) %>%
  left_join(unified_stats %>% dplyr::select(h3_id, hover_label), by = "h3_id")

map_effort_h3 <- map_effort_h3 %>%
  dplyr::select(-any_of("hover_label")) %>%
  left_join(unified_stats %>% dplyr::select(h3_id, hover_label), by = "h3_id")