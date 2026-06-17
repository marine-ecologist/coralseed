# Compare larval reseeding scenarios

## Overview

A useful `coralseed` workflow is to compare alternative release or
biological scenarios. This tutorial demonstrates a repeatable pattern
for running several parameter combinations and summarising expected
settlement outcomes.

``` r
library(coralseed)
library(tidyverse)
library(sf)
library(ggplot2)

sf::sf_use_s2(FALSE)
```

## Load inputs

``` r
benthic_map <- system.file("extdata", "Lizard_Benthic.geojson", package = "coralseed") |>
  st_read(quiet = TRUE)

reef_map <- system.file("extdata", "Lizard_Geomorphic.geojson", package = "coralseed") |>
  st_read(quiet = TRUE)

seascape <- seascape_probability(reefoutline = reef_map, habitat = benthic_map)

particles_sf <- system.file("extdata", "lizard_del_14_1512_sim1_10.json", package = "coralseed") |>
  st_read(quiet = TRUE)
```

## Define scenarios

``` r
scenarios <- tidyr::crossing(
  scenario = c("low_mortality", "medium_mortality", "high_mortality"),
  simulate_mortality_n = c(0.05, 0.10, 0.25)
) |>
  distinct()

scenarios
```

## Scenario runner

``` r
run_scenario <- function(scenario, simulate_mortality_n) {
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
    simulate.mortality.n = simulate_mortality_n,
    return.plot = FALSE,
    return.summary = TRUE,
    silent = TRUE
  )

  settled <- settle_particles(
    seeded,
    probability = "additive",
    return.plot = FALSE,
    silent = TRUE
  )

  tibble(
    scenario = scenario,
    simulate_mortality_n = simulate_mortality_n,
    n_particles = nrow(seeded$points),
    n_settled = nrow(settled$points),
    prop_settled = n_settled / n_particles
  )
}
```

## Run scenarios

``` r
scenario_results <- scenarios |>
  pmap_dfr(run_scenario)

scenario_results
```

For rendered documentation, you can save scenario outputs to `data/` or
`inst/extdata/` and load them here.

## Plot scenario comparison

``` r
ggplot(scenario_results, aes(scenario, prop_settled)) +
  geom_col() +
  labs(
    x = NULL,
    y = "Proportion settled",
    title = "Scenario comparison: expected settlement"
  ) +
  theme_bw()
```

## Extensions:

Potential scenario dimensions:

- release location;
- release timing;
- mortality type;
- competency function;
- release density multiplier;
- post-settlement survival;
- restoration plot size.

Keep parameter sets in a table so each model run is traceable and
reproducible.
