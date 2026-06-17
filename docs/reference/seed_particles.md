# Seed particles

Function to seed particles

## Usage

``` r
seed_particles(
  input = NULL,
  seascape = NULL,
  subsample = NULL,
  simulate.mortality = "none",
  simulate.mortality.n = 0.1,
  brmsfit = infamis_tiles_exp,
  set_b_Intercept = NULL,
  limit.time = NA,
  set.centre = TRUE,
  seed.value = NULL,
  crs = 32755,
  interval = "1 mins",
  silent = TRUE,
  return.summary = FALSE,
  return.plot = FALSE,
  save.plot = FALSE,
  plot.width = 12,
  plot.height = 7.5,
  ...
)
```

## Arguments

- input:

  input

- seascape:

  shp file inputs from seascape_probability()

- subsample:

  subsample to n samples

- simulate.mortality:

  set mortality type via simulate_mortality() one of "typeI","typeII",
  "typeIII" (defaults to "none")

- simulate.mortality.n:

  set proportion of corals to kill over a 24hr period, where 0 is none,
  1 is 100 (defaults to 0.1 or 10%)

- brmsfit:

  a brmsfit model for predicting competency

- set_b_Intercept:

  manually set intercept (for development)

- limit.time:

  limit the time series (e.g. 720 mins)

- set.centre:

  reset CONNIE input to have a central t0 point (defaults to TRUE)

- seed.value:

  set seed for consistent results (defaults to NULL)

- crs:

  coordinate reference system, default EPSG:32755

- interval:

  time interval for interpolating paths

- silent:

  silence printing results (defaults to TRUE)

- return.summary:

  return summary table

- return.plot:

  return plot object

- save.plot:

  path to save plot image

- plot.width:

  width of saved plot

- plot.height:

  height of saved plot

- ...:

  passes functions

## Details

seed_particles() uses predict_competency(), simulate_mortality()

seed_particles(input =
"Users/rof011/coralseed/data-raw/run_day_11656_lizard_fcst_15_2611_26.json",
seascape = seascape, brmsfit=infamis_tiles_exp, simulate.mortality =
"typeI", simulate.mortality.n = 0.1)
