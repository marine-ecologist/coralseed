# Particles to tracks v3.0

Function to convert particles (sf points) to tracks (sf linestrings, or
here a multilinestring)

## Usage

``` r
particles_to_tracks_time(
  input = NULL,
  by = "competency",
  multilinestring = TRUE
)
```

## Arguments

- input:

  input (defaults to NULL)

- by:

  factor level

- multilinestring:

  "LINESTRING" (FALSE) or "MULTILINESTRING" (TRUE)

## Details

Avoids the issue below: note: GEOS throws an error when n points is less
than 3. When using the by argument the function drops levels with less
than 3. This isn't an issue for mapping as only removes late competency
particles, but check and be careful converting other factors to paths:
https://gis.stackexchange.com/questions/447578/geosexception-illegalargumentexception-point-array-must-contain-0-or-1-elemen

!!sym(by)
