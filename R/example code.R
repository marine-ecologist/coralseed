
library(tidyverse)
library(sf)
library(tmap)

run_day_12038_liz22_del_14_33 <- st_read("/Users/rof011/coralseed/code/2021_2022_HighPRT_sim_examples/run_day_12038_liz22_del_14_33/run_day_12038_liz22_del_14_33.json")


run_day_12038_liz22_del_14_33_subset <- run_day_12038_liz22_del_14_33 |> dplyr::filter(id %in% seq(0:60))

tm_shape(run_day_12038_liz22_del_14_33_subset) +
  tm_dots("time", pal="-Spectral", legend.show=TRUE)

