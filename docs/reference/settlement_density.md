# Settlement density

Function to calculate settlement density across a grid Uses sp::over
instead of sf for speed

## Usage

``` r
settlement_density(
  input = NULL,
  messages = FALSE,
  calculate_hull = TRUE,
  combined = FALSE,
  cellsize = 20,
  concavity = 1.2,
  length_threshold = 10,
  ...
)
```

## Arguments

- input:

  input (defaults to NULL)

- messages:

  print progress messages (default FALSE)

- calculate_hull:

  true/false calculate concave hull (can be time intensive on large
  datasets, defaults to true)

- combined:

  test to allow multiple coralseed file inputs in list (defaults to
  FALSE)

- cellsize:

  dimensions of grid to count settlers (defaults to 20m)

- concavity:

  concavity value passed to concaveman (defaults to 2

- length_threshold:

  length threshold passed to concaveman (defaults to 0)

- ...:

  pass arguments
