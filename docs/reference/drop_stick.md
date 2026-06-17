# Drop stuck particles

Function to remove particles that remain at the same location for more
than `n_time` consecutive timesteps.

## Usage

``` r
drop_stick(input, n_time = 100, saveoutput = NULL)
```

## Arguments

- input:

  input `sf` object containing particle tracks with time and id columns

- n_time:

  number of consecutive identical positions to consider as "stuck"
  (default: 100)

- saveoutput:

  optional file path to export the filtered dataset as GeoJSON
