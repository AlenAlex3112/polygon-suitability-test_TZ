
grid_stats <- data_req %>%
  group_by(h3_id) %>%
  mutate(total_lists = n_distinct(GROUP.ID)) %>%
  ungroup() %>%
  group_by(h3_id, total_lists, COMMON.NAME, polygon_id) %>%
  summarise(detections = n_distinct(GROUP.ID), .groups = "drop") %>%
  mutate(frequency = detections / total_lists) %>%
  group_by(h3_id) %>%
  arrange(desc(frequency)) %>%
  filter(frequency >= 0.05) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  as.data.frame()

polygon_stats <- data_req %>%
  group_by(polygon_id) %>%
  mutate(total_lists = n_distinct(GROUP.ID)) %>%
  ungroup() %>%
  group_by(polygon_id, total_lists, COMMON.NAME) %>%
  summarise(detections = n_distinct(GROUP.ID), .groups = "drop") %>%
  mutate(frequency = detections / total_lists) %>%
  group_by(polygon_id) %>%
  arrange(desc(frequency)) %>%
  filter(frequency >= 0.01) %>% 
  mutate(rank = row_number()) %>%
  ungroup() %>%
  as.data.frame()

polygon_lookup <- map %>% st_drop_geometry() %>% dplyr::select(polygon_id = id, subregion) %>% distinct()

grid_counts <- data_req %>%
  group_by(h3_id, polygon_id) %>%  
  reframe(unique_species = n_distinct(COMMON.NAME)) |> 
  group_by(h3_id) %>%
  slice_max(order_by = unique_species, n = 1, with_ties = FALSE) %>%
  ungroup()

grid_effort <- data_req %>%
  group_by(h3_id, polygon_id) %>%  
  reframe(checklist_count = n_distinct(GROUP.ID)) |> 
  group_by(h3_id) %>%
  slice_max(order_by = checklist_count, n = 1, with_ties = FALSE) %>%
  ungroup() |> 
  group_by(polygon_id) %>%
  mutate(
    poly_tot_lists = sum(checklist_count),
    effort_percentage = (checklist_count / poly_tot_lists) * 100
  ) %>%
  ungroup()

# grid_effort$effort_percentage = 50