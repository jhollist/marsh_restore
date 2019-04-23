#browser()

max_row_idx <- profiledf %>% 
  group_by(habitat_13_id) %>% 
  mutate(grp_max = max(distance)) %>% 
  ungroup() %>% 
  mutate(grp_max_logic = distance == grp_max) %>%
  pull(grp_max_logic) %>%
  which

max_row_idx <- max_row_idx[-length(max_row_idx)]

insert_me <- profiledf %>% 
  slice(max_row_idx) %>%
  mutate(habitat_agg = NA)

profiledf <- profiledf %>%
  rbind(insert_me) %>%
  arrange(transect, year, distance) %>%
  mutate(habitat_agg = zoo::na.locf(habitat_agg, na.rm = FALSE, 
                                    fromLast = TRUE))