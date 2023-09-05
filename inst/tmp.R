
# remove identical geometry for each id, retain the first instance
tmp <- particles %>% 
  group_by(id, geometry) %>% 
  slice_head(n = 1) %>% 
  ungroup()

tmp

# tmp %>%
#   group_by(id, geometry) %>%
#   summarise(n_duplicated = n()) %>%
#   filter(n_duplicated > 1) %>%
#   ungroup()


tmp2 <- tmp %>% 
  arrange(id) %>% # Ensure data is sorted by id for lag function
  group_by(id, competency) %>%
  mutate(
    geometry_lagged = lag(geometry, default =  st_as_sfc("POINT(EMPTY)", crs = 20353))
  ) %>% 
  slice(-1) %>%
  ungroup() %>%
  mutate(
    line = st_sfc(purrr::map2(
      .x = geometry, 
      .y = geometry_lagged, 
      .f = ~{st_union(c(.x, .y)) %>% st_cast("LINESTRING")}
    ))) %>% 
  select(id, line, competency, dispersaltime)

tmp3 <- tmp2 %>%
  st_sf(geometry = st_sfc(tmp2$line, crs = st_crs(tmp2))) |> 
  arrange(id, dispersaltime)
