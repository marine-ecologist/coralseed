# Tiltmeter-derived flow fields and particle simulations

## Overview

This tutorial covers the tiltmeter workflow added to `coralseed`:

1.  aggregate tiltmeter velocities in time;
2.  interpolate eastward and northward velocities onto a grid;
3.  simulate 2D stochastic Lagrangian particle trajectories through the
    gridded flow field;
4.  visualise flow fields and particle tracks.

``` r
library(coralseed)
library(tidyverse)
library(sf)
library(ggplot2)
library(lubridate)
library(terra)

sf::sf_use_s2(FALSE)
```

## Required tiltmeter input

The tiltmeter input should be an `sf` point object with these columns:

``` r
required_cols <- c(
  "iso_8601_time",
  "speed_cm_s",
  "heading_degrees",
  "velocity_n_cm_s",
  "velocity_e_cm_s",
  "inst",
  "time",
  "lat",
  "lon"
)
```

Example preparation:

``` r
tiltmeters <- tiltmeters |>
  mutate(
    time = ymd_hms(iso_8601_time, tz = "UTC"),
    inst = as.character(inst)
  ) |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
```

## Inspect tiltmeter time series

``` r
tiltmeters |>
  st_drop_geometry() |>
  mutate(inst = forcats::fct_reorder(inst, lat, .fun = mean, .desc = FALSE)) |>
  ggplot(aes(time, speed_cm_s, group = inst)) +
  geom_line() +
  facet_wrap(~ inst, nrow = 1) +
  labs(
    x = NULL,
    y = "Speed (cm/s)",
    title = "Tiltmeter speed time series ordered by latitude"
  ) +
  theme_bw()
```

## Build flow fields

[`simulate_flowfields()`](https://marine-ecologist.github.io/coralseed/reference/simulate_flowfields.md)
converts tiltmeter velocities from cm/s to m/s, aggregates to a time
interval, builds a spatial grid, and interpolates `u_ms` and `v_ms`
separately.

``` r
flow_sim <- simulate_flowfields(
  tiltmeters = tiltmeters,
  gbr_buffer = gbr_buffer,
  crs_m = 32755,
  grid_res_m = 10,
  buffer_m = 250,
  time_unit = "5 minutes",
  idp = 2
)

flow_sim$flow_field
flow_sim$grid_sf
```

## Plot one flow-field timestep

``` r
flow_plot <- flow_sim$flow_field |>
  filter(time == ymd_hms("2025-11-17T11:30:00", tz = "UTC")) |>
  mutate(
    xend = x + u_ms * 2000,
    yend = y + v_ms * 2000
  )

ggplot() +
  geom_sf(data = flow_plot, aes(colour = speed_ms), size = 2) +
  geom_segment(
    data = st_drop_geometry(flow_plot),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.08, "inches")),
    linewidth = 0.25,
    alpha = 0.4
  ) +
  scale_colour_viridis_c() +
  coord_sf(crs = st_crs(32755)) +
  theme_bw()
```

## Rasterise flow fields with terra

``` r
grid_v <- terra::vect(flow_sim$grid_sf)

r_template <- terra::rast(
  terra::ext(grid_v),
  resolution = 10,
  crs = terra::crs(grid_v)
)

flow_v <- flow_plot |>
  st_drop_geometry() |>
  terra::vect(
    geom = c("x", "y"),
    crs = sf::st_crs(flow_plot)$wkt
  )

speed_r <- terra::rasterize(
  x = flow_v,
  y = r_template,
  field = "speed_ms",
  fun = "mean"
)

plot(speed_r)
```

## Define release sites

``` r
release_sites <- tribble(
  ~lat,        ~lon,
  -20.0612,   148.9605,
  -20.0614,   148.9615,
  -20.06125,  148.9625
) |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
```

## Run 2D stochastic Lagrangian particle simulation

``` r
particle_sim <- simulate_particles(
  flow_field = flow_sim$flow_field,
  land_poly = land_poly,
  release_sites = release_sites,
  crs_m = 32755,
  dt = 5 * 60,
  release_duration_seconds = 60 * 60,
  release_by = "60 sec",
  n_particles_per_site_per_release = 100,
  K = 0.05,
  max_land_retry = 200,
  seed = 101,
  parallel = TRUE,
  workers = 6
)

particle_sim$particle_track_summary
```

## Plot particle tracks

``` r
track_sample <- particle_sim$particle_tracks_4326 |>
  filter(particle_id %in% sample(unique(particle_id), 500))

ggplot() +
  geom_sf(data = land_poly, fill = "grey40", colour = NA) +
  geom_sf(
    data = track_sample,
    aes(colour = factor(site_id)),
    size = 0.25,
    alpha = 0.25,
    show.legend = FALSE
  ) +
  geom_sf(data = release_sites, shape = 21, fill = "red", size = 3) +
  theme_bw() +
  coord_sf(expand = FALSE)
```

## Interpretation

This is an offline 2D stochastic Lagrangian particle-tracking model. The
flow field is not dynamically solved; it is interpolated from fixed
tiltmeter measurements. This makes it useful for diagnostic, near-field
dispersal exploration, but it should not be described as a full
hydrodynamic model.
