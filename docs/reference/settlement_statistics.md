# Settlement statistics

Function to generate settlement statistics from coralseed output

## Usage

``` r
settlement_statistics(
  input = NULL,
  combined = FALSE,
  cellsize = 20,
  concavehull = TRUE,
  concavity = 2,
  length_threshold = 0,
  ...
)
```

## Arguments

- input:

  input (defaults to NULL)

- combined:

  test to allow multiple coralseed file inputs in list (defaults to
  FALSE)

- cellsize:

  dimensions of grid to count settlers (defaults to 20m)

- concavehull:

  return concave hull around settled particles (via concaveman)

- concavity:

  concavity value passed to concaveman (defaults to 2

- length_threshold:

  length threshold passed to concaveman (defaults to 0)

- ...:

  pass arguments
