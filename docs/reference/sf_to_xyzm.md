# Build LINESTRING XYZM trips (integer M from a time column)

Minimal helper to create LINESTRING XYZM for mapdeck::add_trips() using
an integer range from a time/index column (e.g. "dispersaltime").

- type = "points": group points by `group_col` (default "id") ordered by
  time, build one LINESTRING per group with M = integer(time).

- type = "linestring": if `id_col` present, collapse time-sliced
  features per id and ramp M linearly from t -\> next_t along vertices;
  last slice uses next_t = t + 1. If no `id_col`, each feature ramps t
  -\> t+1.

## Usage

``` r
sf_to_xyzm(sf_obj, time_col = "dispersaltime", z_val = 0, cast_multi = TRUE)
```

## Arguments

- sf_obj:

  sf object with LINESTRING/MULTILINESTRING geometry

- time_col:

  character name of time/index column (integers or numeric)

- z_val:

  numeric constant Z (default 0)

- cast_multi:

  logical; cast MULTILINESTRING to LINESTRING (default TRUE)

## Details

Sets geometry m_range attribute required by mapdeck.
