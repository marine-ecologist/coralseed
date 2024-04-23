library(tidync)
library(tidyverse)
library(sf)
library(tmap)


####### plot reef polygons

reef_polygons <- st_read("/Users/rof011/Downloads/GBR_connectivity/datafiles/MooreCluster_SpatialPolygons.gpkg") |>
  mutate(reef_names = sub("_.*", "", Reef))

reef_names <- reef_polygons |>
  group_by(reef_names) %>%
  summarize(geometry = st_union(geom)) %>%
  st_centroid()

tmap_mode("plot")
tm_shape(reef_polygons) +
  tm_polygons("habitat",
              fill_alpha=0.2,
              lwd=0.05,
              id="site_id") +
tm_shape(reef_names) +
  tm_labels("reef_names")



####### import Oceanparcels output:


library(reticulate)
zarr <- import("zarr")
xarray <- import("xarray")


ds <- xarray$open_zarr("/Users/rof011/Mooreoutputs2015Moore_trajectory_2015_d12d.zarr..zarr/")



particle_tracks_moore_2015 <- tidync("/Users/rof011/parcels/output.nc") %>%
  hyper_tibble(select = c("time", "lon", "lat")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

particle_tracks_moore_2015 |> filter(trajectory==1000) |> tail()




tm_shape(reef_polygons) +
  tm_polygons("habitat",
              fill_alpha=0.2,
              lwd=0.05,
              id="site_id")

tm_shape(particle_tracks_moore_2015 |> filter(trajectory == 1)) +
  tm_dots("trajectory")

# make particle tracks
particle_tracks_lines <- particle_tracks_moore_2015 %>%
  group_by(trajectory) %>%
  summarize(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")





# RECOM time 9404 to 9525
tmp <- tidync("/Users/rof011/Downloads/GBR_connectivity/datafiles/Moore_2015_simple.nc")

# Oceanparcels 9464 to 9465
tmp <- tidync("/Users/rof011/Downloads/GBR_connectivity/Oceanparcels/Moore_uv_2015-11-30.nc")



library(lubridate)
library(tidyverse)
library(sf)

# install Rarr for handling Ocean Parcels Zarr format
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
library(Rarr)

ocean_parcels_particles <- read.csv("/Users/rof011/trajectories_obs_data.csv")

zarr_overview

