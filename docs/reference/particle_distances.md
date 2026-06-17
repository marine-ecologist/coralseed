# Particle distances

Function to calculate distances of particles at any timepoint limit the
particle distances to tmax (in minutes, e.g. 60 for 1hr) returns either
"sf" ("MULTILINGSTRING" for each id) or "df" (data.frame with total
length for each id) depending on type="df" or type="sf"

## Usage

``` r
particle_distances(input = NULL, tmax = NULL, type = "df", ...)
```

## Arguments

- input:

  input (defaults to NULL)

- tmax:

  limit particle times to less than tmax

- type:

  export format to "sf" or "df" (see above)

- ...:

  pass arguments
