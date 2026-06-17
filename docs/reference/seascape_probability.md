# Seascape Probability

Function to generate spatial settlement probability layers from Allen
Coral Atlas maps.

## Usage

``` r
seascape_probability(
  reefoutline = benthic_map,
  habitat = reef_map,
  probability = NULL,
  ...
)
```

## Arguments

- reefoutline:

  `sf` object (or filepath) for Coral Atlas benthic habitat (reef
  outline)

- habitat:

  `sf` object (or filepath) for Coral Atlas geomorphic classes

- probability:

  Optional data.frame with `class`, `means`, and `se` columns for custom
  settlement probabilities. If NULL, defaults are used.

- ...:

  Not used. Placeholder for extensibility.

## Value

`sf` object with `class`, `habitat_id`, and `settlement_probability`
