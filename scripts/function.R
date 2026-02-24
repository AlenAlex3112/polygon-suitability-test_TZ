calc_outliers <- function(target_h3, target_poly, grid_df, poly_df) {
  local_species <- grid_df %>% filter(h3_id == target_h3) %>% pull(COMMON.NAME)
  poly_freq  <- poly_df %>% filter(polygon_id == target_poly) %>% pull(COMMON.NAME)
  outliers <- setdiff(local_species, poly_freq)
  return(tibble(outlier_count = length(outliers),
                outlier_list = if(length(outliers) > 0) paste(sort(outliers), collapse = ", ") else "None"))
}

calc_outlier_exclusion <- function(target_h3, target_poly, data_df) {

  local_data <- data_df %>%
  filter(h3_id == target_h3)

  total_lists_local <- n_distinct(local_data$GROUP.ID)

  local_species <- local_data %>%
    group_by(COMMON.NAME) %>%
    summarise(freq = n_distinct(GROUP.ID) / total_lists_local, .groups = "drop") %>%
    filter(freq >= 0.1) %>%
    pull(COMMON.NAME)

  poly_excluded_data <- data_df %>%
    filter(polygon_id == target_poly, h3_id != target_h3)

  total_lists_poly_ex <- n_distinct(poly_excluded_data$GROUP.ID)

  poly_freq_ex <- poly_excluded_data %>%
    group_by(COMMON.NAME) %>%
    summarise(freq = n_distinct(GROUP.ID) / total_lists_poly_ex, .groups = "drop") %>%
    filter(freq >= 0.01) %>%
    pull(COMMON.NAME)

  outliers <- setdiff(local_species, poly_freq_ex)

  return(tibble(
    outlier_count = length(outliers),
    outlier_list = if(length(outliers) > 0) paste(sort(outliers), collapse = ", ") else "None"
  ))
}

results <- grid_effort %>%
  group_by(h3_id, polygon_id) %>%
  mutate(
    outlier_data = if (effort_percentage <= 10 & checklist_count >= 20) {
      calc_outlier_exclusion(h3_id, polygon_id, data_req) 
    } else {
      calc_outliers(h3_id, polygon_id, grid_stats, polygon_stats)
    }
  ) %>%
  unnest(outlier_data) %>%
  ungroup()

