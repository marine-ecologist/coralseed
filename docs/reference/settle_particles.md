# Settle Particles

Function to determine probability of settlement if particles pass
suitable substrate. Options:

- "additive": settle somewhere in habitat intersect regardless of time

- "lagged": sample first 10 minutes of habitat entry

- "rapid": settle at first habitat intersect

## Usage

``` r
settle_particles(
  input,
  seascape = seascape,
  probability = "additive",
  silent = TRUE,
  return.plot = FALSE,
  subsample = NULL,
  seed.value = NULL,
  ...
)
```

## Arguments

- input:

  output from
  [`seed_particles()`](https://marine-ecologist.github.io/coralseed/reference/seed_particles.md)
  or directly a particle `sf` object

- seascape:

  sf object from
  [`seascape_probability()`](https://marine-ecologist.github.io/coralseed/reference/seascape_probability.md),
  for plotting base

- probability:

  settlement rule, one of "additive", "rapid", or "lagged"

- silent:

  suppress printed output (default TRUE)

- return.plot:

  whether to return tmap plot output

- subsample:

  optionally sample N particles for faster plotting

- seed.value:

  set seed for reproducibility

- ...:

  additional args
