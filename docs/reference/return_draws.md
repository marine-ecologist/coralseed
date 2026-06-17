# return draws

Function to return draws from time-to-competency brms model

## Usage

``` r
return_draws(input, fit, tmax = 12, by = 1, n = 1000, ...)
```

## Arguments

- input:

  brms model fit

- fit:

  distribution family: "exp", "weibull", or "lognormal"

- tmax:

  max time in hours

- by:

  seq by in minutes

- n:

  number of simulations

- ...:

  additional arguments (unused)
