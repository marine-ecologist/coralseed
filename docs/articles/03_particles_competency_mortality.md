# Seed particles, competency, and mortality

## Overview

This tutorial focuses on
[`seed_particles()`](https://marine-ecologist.github.io/coralseed/reference/seed_particles.md):
the step that converts particle-track output into biologically filtered
larval particles by applying dispersal-time limits, competency,
mortality, and habitat settlement probability.

``` r
library(coralseed)
library(tidyverse)
library(sf)
library(ggplot2)

sf::sf_use_s2(FALSE)
```

## Prepare seascape and particle inputs

``` r
benthic_map <- system.file("extdata", "Lizard_Benthic.geojson", package = "coralseed") |>
  st_read(quiet = TRUE)

reef_map <- system.file("extdata", "Lizard_Geomorphic.geojson", package = "coralseed") |>
  st_read(quiet = TRUE)

seascape <- seascape_probability(
  reefoutline = reef_map,
  habitat = benthic_map
)

particles_sf <- system.file("extdata", "lizard_del_14_1512_sim1_10.json", package = "coralseed") |>
  st_read(quiet = TRUE)
```

## Run a baseline seed-particle model

``` r
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
  return.plot = TRUE,
  return.summary = TRUE,
  silent = FALSE
)
```

## Inspect outputs

``` r
names(seeded)
```

    [1] "seed_particles" "multiplot"      "summary"       

``` r
seeded$summary
```

                         Metric               Value
    1 Number of particle tracks                1000
    2              Seed setting       [No seed set]
    3                Start time          2022-12-16
    4                  End time 2022-12-16 12:57:00
    5      Dispersal time (hrs)               12.95
    6   Total mortality by tmax                  79

If the returned object includes points, inspect key fields:

``` r
seeded$points |>
  st_drop_geometry() |>
  glimpse()
```

     NULL

## Compare competency functions

Use a small scenario grid to compare model behaviour.

``` r
competency_models <- c("exponential", "weibull", "lognormal")

seeded_competency <- purrr::map(
  competency_models,
  \(model) {
    seed_particles(
      input = particles_sf,
      zarr = FALSE,
      set.centre = TRUE,
      seascape = seascape,
      probability = "additive",
      limit.time = 12,
      competency.function = model,
      crs = 20353,
      simulate.mortality = "typeIII",
      simulate.mortality.n = 0.1,
      return.plot = FALSE,
      return.summary = TRUE,
      silent = TRUE
    )
  }
) |>
  set_names(competency_models)
```

## Compare mortality curves

``` r
mortality_models <- c("typeI", "typeII", "typeIII")

seeded_mortality <- purrr::map(
  mortality_models,
  \(mortality) {
    seed_particles(
      input = particles_sf,
      zarr = FALSE,
      set.centre = TRUE,
      seascape = seascape,
      probability = "additive",
      limit.time = 12,
      competency.function = "exponential",
      crs = 20353,
      simulate.mortality = mortality,
      simulate.mortality.n = 0.1,
      return.plot = FALSE,
      return.summary = TRUE,
      silent = TRUE
    )
  }
) |>
  set_names(mortality_models)
```

## Extract particle tracks

Use
[`particles_to_tracks()`](https://marine-ecologist.github.io/coralseed/reference/particles_to_tracks.md)
or related helpers when you need line features for plotting or spatial
summaries.

``` r
tracks <- particles_to_tracks(seeded$points)
tracks
```
