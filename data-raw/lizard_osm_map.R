## code to prepare `lizard_osm_map` dataset goes here


osm_data <- opq(bbox =  c(145.42, -14.64, 145.48, -14.72)) %>%
  add_osm_feature(key = 'natural') %>%
  osmdata_sf()

lizard_osm_map <- osm_data$osm_polygons |>
  dplyr::filter(natural=="coastline") |>
  select(natural) |>
  st_transform(20353)


usethis::use_data(lizard_osm_map, overwrite = TRUE)
