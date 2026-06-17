# Set restoration plot

Function to create a rectangular polygon around the release point
(centroid or t0 of particle release) for plotting in tmap

## Usage

``` r
set_restoration_plot(
  input = NULL,
  width = NULL,
  length = NULL,
  crs = 4326,
  center = "coralseed"
)
```

## Arguments

- input:

  input `sf` object used to derive the center

- width:

  width in metres

- length:

  length in metres

- crs:

  CRS for output object (default 4326)

- center:

  either "coralseed" for t0-based centroid, or "sf" for direct centroid
