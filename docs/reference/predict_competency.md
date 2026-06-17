# Predict competency

Function to generate time-to-settlement probability distributions using
a `brms` model

## Usage

``` r
predict_competency(
  input,
  n_particles = 100,
  max.time = NULL,
  seed.value = NULL,
  return.plot = FALSE,
  ...
)
```

## Arguments

- input:

  brmsfit object from a model using 'lognormal', 'exponential', or
  'weibull' family

- n_particles:

  number of random simulations

- max.time:

  optional maximum dispersal time limit (default is max time in
  input\$data)

- seed.value:

  random seed for reproducibility

- return.plot:

  if TRUE, returns ggplot of survival curves + point estimates

- ...:

  additional arguments (not used)

## Details

Examples: predict_competency(infamis_tiles_exp, n_particles = 100)
predict_competency(infamis_tiles_weibull, n_particles = 100)
predict_competency(infamis_tiles_log, n_particles = 100)
