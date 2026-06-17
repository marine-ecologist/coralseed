# Flowchart of Coral Larval Fates

Generate an interactive Sankey diagram showing the fate of coral larvae
through dispersal, settlement, and mortality.

## Usage

``` r
flowchart_coralseed(
  seed_particles_input,
  settle_particles_input,
  save_output = NULL,
  multiplier = 1,
  postsettlement = 0.8,
  width = 10,
  height = 6
)
```

## Arguments

- seed_particles_input:

  An `sf` object or tibble containing all released larval trajectories
  with at least `id`, `dispersaltime`, and `state`.

- settle_particles_input:

  A named list containing `$points`, an `sf` object with settled larvae
  and `id`.

- save_output:

  optional file path to save output as png, jpg, or pdf

- multiplier:

  Numeric multiplier to scale particle counts (e.g., if each simulated
  particle represents multiple larvae).

- postsettlement:

  Postsettlement mortality in proportion (e.g. 80% = 0.8)

- width:

  output width in inches (default 10)

- height:

  output height in inches (default 6)

## Value

An interactive Sankey diagram (htmlwidget).

## Examples

``` r
if (FALSE) { # \dontrun{
flowchart_coralseed(moore_particles, moore_settlers, multiplier = 1, postsettlement = 0.8)
} # }


```
