# Simulate Mortality

Function to simulate mortality of a population. `simulate.mortality.n`
is the proportion of the population to simulate mortality over a 24hr
period.

## Usage

``` r
simulate_mortality(
  input = NULL,
  simulate.mortality = NULL,
  simulate.mortality.n = 0.1,
  return.plot = TRUE,
  silent = FALSE,
  seed.value = NULL,
  ...
)
```

## Arguments

- input:

  input (e.g. from
  [`seed_particles()`](https://marine-ecologist.github.io/coralseed/reference/seed_particles.md),
  usually a time-expanded `sf` object)

- simulate.mortality:

  select mortality type ("typeI", "typeII", or "typeIII")

- simulate.mortality.n:

  proportion of population to simulate mortality on (0–1)

- return.plot:

  generate plot output (default: TRUE)

- silent:

  suppress printed messages (default: FALSE)

- seed.value:

  seed for reproducibility (default: NULL)

- ...:

  pass additional args

## Details

Mortality types I, II, III are Weibull-distributed. Set
`return.plot=TRUE` to visualise simulation vs empirical.

Example: tmp \<- simulate_mortality(particle_points_expanded,
simulate.mortality = "typeIII", simulate.mortality.n = 0.2, return.plot
= TRUE)
