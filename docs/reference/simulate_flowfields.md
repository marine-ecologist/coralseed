# Simulate interpolated flow fields from tiltmeter velocity data

Interpolates eastward and northward velocity components from tiltmeter
observations onto a regular spatial grid for each time step. The
function aggregates tiltmeter observations to a user-defined temporal
resolution, converts velocities from cm/s to m/s, creates a buffered
spatial domain around the instruments, and uses inverse distance
weighting to generate gridded flow fields.

## Usage

``` r
simulate_flowfields(
  tiltmeters,
  gbr_buffer,
  crs_m = 32755,
  grid_res_m = 10,
  buffer_m = 250,
  time_unit = "5 minutes",
  idp = 2
)
```

## Arguments

- tiltmeters:

  An `sf` object or data frame containing tiltmeter observations. Must
  contain columns `time`, `inst`, `lat`, `lon`, `velocity_e_cm_s`,
  `velocity_n_cm_s`, and `speed_cm_s`.

- gbr_buffer:

  An `sf` polygon object defining the spatial mask or reef buffer used
  to constrain the interpolation domain.

- crs_m:

  Numeric or character. Projected coordinate reference system used for
  interpolation in metres. Default is `32755`.

- grid_res_m:

  Numeric. Grid cell spacing in metres. Default is `10`.

- buffer_m:

  Numeric. Buffer distance in metres around the convex hull of tiltmeter
  locations. Default is `250`.

- time_unit:

  Character. Temporal aggregation unit passed to
  [`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html).
  Default is `"5 minutes"`.

- idp:

  Numeric. Inverse distance weighting power passed to
  [`gstat::idw()`](https://r-spatial.github.io/gstat/reference/krige.html).
  Higher values give more weight to nearby observations. Default is `2`.

## Value

A named list with:

- flow_field:

  An `sf` point object in `crs_m` containing interpolated `u_ms`,
  `v_ms`, `speed_ms`, and `heading_degrees` for each grid cell and time
  step.

- flow_field_4326:

  The same interpolated flow field transformed to EPSG:4326.

- tiltmeters_m:

  Aggregated tiltmeter observations transformed to `crs_m`.

- grid_sf:

  The interpolation grid as an `sf` point object in `crs_m`.

- domain_m:

  The buffered interpolation domain clipped to `gbr_buffer`.

## Details

The function first aggregates tiltmeter measurements by instrument and
floored time interval. Eastward and northward velocities are converted
from cm/s to m/s. A convex hull is constructed around the tiltmeter
locations, buffered by `buffer_m`, and clipped to `gbr_buffer`. A
regular point grid is then generated over this domain.

For each time step, inverse distance weighting is applied separately to
the eastward (`u_ms`) and northward (`v_ms`) velocity components.
Wind/current speed is then calculated as:

\$\$speed = \sqrt{u^2 + v^2}\$\$

Heading is calculated in degrees using:

\$\$heading = atan2(u, v)\$\$

and converted to compass-style degrees from 0 to 360.

Time steps with fewer than three tiltmeter observations are skipped.

## Examples

``` r
if (FALSE) { # \dontrun{
flow_sim <- simulate_flowfields(
  tiltmeters = tiltmeters,
  gbr_buffer = gbr_buffer,
  crs_m = 32755,
  grid_res_m = 10,
  buffer_m = 250,
  time_unit = "5 minutes",
  idp = 2
)

flow_sim$flow_field
flow_sim$flow_field_4326
} # }
```
