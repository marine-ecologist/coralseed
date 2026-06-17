# Visualising coralseed outputs

## Overview

This tutorial collects visualisation patterns for `coralseed` outputs:
static `ggplot2` maps, package plotting functions,
[`map_coralseed()`](https://marine-ecologist.github.io/coralseed/reference/extract_parallel.md)
widgets, flowcharts, and animations.

``` r
library(coralseed)
library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)

sf::sf_use_s2(FALSE)
```

## Build a small example simulation

``` r
benthic_map <- system.file("extdata", "Lizard_Benthic.geojson", package = "coralseed") |>
  st_read(quiet = TRUE)

reef_map <- system.file("extdata", "Lizard_Geomorphic.geojson", package = "coralseed") |>
  st_read(quiet = TRUE)

seascape <- seascape_probability(reefoutline = reef_map, habitat = benthic_map)

particles_sf <- system.file("extdata", "lizard_del_14_1512_sim1_10.json", package = "coralseed") |>
  st_read(quiet = TRUE)

seeded <- seed_particles(
  input = particles_sf,
  zarr = FALSE,
  set.centre = TRUE,
  seascape = seascape,
  probability = "additive",
  limit.time = 12,
  competency.function = "exponential",
  crs = 20353,
  simulate.mortality = "typeIII",
  simulate.mortality.n = 0.1,
  return.plot = FALSE,
  return.summary = TRUE,
  silent = TRUE
)

settled <- settle_particles(seeded, probability = "additive", return.plot = FALSE, silent = TRUE)
settlement_dens <- settlement_density(settled$points)
```

## Static settlement map

``` r
ggplot() +
  geom_sf(data = seascape, aes(fill = settlement_probability), colour = NA, alpha = 0.55) +
  geom_sf(data = settled$points, size = 0.2, alpha = 0.35) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(
    title = "Settled particles over habitat probability surface",
    fill = "Settlement\nprobability"
  ) +
  theme_bw()
```

## Density map

``` r
ggplot() +
  geom_sf(data = seascape, fill = "grey92", colour = NA) +
  geom_sf(data = settlement_dens, aes(colour = n), size = 1.5) +
  scale_colour_viridis_c() +
  labs(
    title = "Settlement density",
    colour = "Settlers"
  ) +
  theme_bw()
```

## Package plot helper

``` r
plot_particles(settled$points, seascape)
```

## Interactive map

[`map_coralseed()`](https://marine-ecologist.github.io/coralseed/reference/extract_parallel.md)
creates an interactive output. Keep it `eval: false` in pkgdown if the
widget is slow or unstable during rendering.

``` r
map_coralseed(
  seed_particles_input = seeded,
  settle_particles_input = settled,
  settlement_density_input = settlement_dens,
  seascape_probability = seascape,
  restoration.plot = c(100, 100),
  show.footprint = TRUE,
  show.tracks = TRUE,
  subsample = 1000,
  webGL = TRUE
)
```

## Flowchart

``` r
flowchart_coralseed(
  seeded,
  settled,
  multiplier = 1000,
  postsettlement = 0.8
)
```

## Animation

Use
[`animate_coralseed()`](https://marine-ecologist.github.io/coralseed/reference/animate_coralseed.md)
for communication outputs. Keep animation chunks unevaluated in pkgdown
unless the output is cached and intentionally committed.

``` r
animate_coralseed(
  seed_particles_input = seeded,
  settle_particles_input = settled,
  seascape_probability = seascape,
  output = "outputs/lizard_particle_animation.gif"
)
```
