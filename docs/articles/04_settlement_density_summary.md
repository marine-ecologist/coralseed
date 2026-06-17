# Settlement, density, and spatial summaries

## Overview

This tutorial covers the settlement end of the workflow:
[`settle_particles()`](https://marine-ecologist.github.io/coralseed/reference/settle_particles.md),
[`settlement_density()`](https://marine-ecologist.github.io/coralseed/reference/settlement_density.md),
[`settlement_summary()`](https://marine-ecologist.github.io/coralseed/reference/settlement_summary.md),
and visual summaries of settled larvae.

``` r
library(coralseed)
library(tidyverse)
library(sf)
library(ggplot2)

sf::sf_use_s2(FALSE)
```

## Recreate baseline inputs

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
```

## Simulate settlement

``` r
settled <- settle_particles(
  seeded,
  probability = "additive",
  return.plot = FALSE,
  silent = TRUE
)

settled$points
```

## Plot settled particles over the seascape

``` r
plot_particles(settled$points, seascape)
```

## Settlement density

``` r
settlement_dens <- settlement_density(settled$points)
settlement_dens
```

``` r
ggplot() +
  geom_sf(data = seascape, aes(fill = settlement_probability), colour = NA, alpha = 0.5) +
  geom_sf(data = settlement_dens, aes(colour = n), size = 1) +
  scale_fill_viridis_c(na.value = "grey90") +
  scale_colour_viridis_c() +
  labs(
    fill = "Settlement\nprobability",
    colour = "Settlers",
    title = "Spatial density of settled particles"
  ) +
  theme_bw()
```

## Spatial summary table

``` r
settlement_sum <- settlement_summary(
  seeded,
  settled,
  cellsize = 50
)

settlement_sum
```

## Summarise by distance from release centre

``` r
release_centre <- seeded$centre

settlement_distance <- settled$points |>
  st_transform(20353) |>
  mutate(distance_m = as.numeric(st_distance(geometry, st_transform(release_centre, 20353)))) |>
  st_drop_geometry() |>
  mutate(distance_bin_m = cut(distance_m, breaks = seq(0, max(distance_m, na.rm = TRUE), by = 50))) |>
  count(distance_bin_m)

settlement_distance
```
