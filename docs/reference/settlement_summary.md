# Summarise settlement results

Computes summary statistics on larval settlement outcomes including
spatial footprint, distance travelled, density, and spatial distribution
metrics.

## Usage

``` r
settlement_summary(seeded_particles, settled_particles, cellsize, ...)
```

## Arguments

- seeded_particles:

  List containing seed_particles data frame with settlement time.

- settled_particles:

  List containing settled particles with `$points` (sf POINT) and
  `$paths` (distance info).

- cellsize:

  Numeric grid resolution used for spatial footprint calculation.

- ...:

  Additional arguments passed to settlement_statistics().

## Value

A data frame of summary statistics.
