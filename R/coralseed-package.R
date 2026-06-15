#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table data.table
#' @importFrom dplyr do
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr slice
#' @importFrom dplyr slice_head
#' @importFrom dplyr slice_sample
#' @importFrom grDevices heat.colors
#' @importFrom lubridate minutes
#' @importFrom lubridate ymd_hms
#' @importFrom methods as
#' @importFrom purrr map
#' @importFrom rlang syms
#' @importFrom sf sf.colors
#' @importFrom sf sf_use_s2
#' @importFrom sf st_as_sfc
#' @importFrom sf st_crs
#' @importFrom sf st_geometry
#' @importFrom sf st_is_valid
#' @importFrom sf st_point
#' @importFrom sf st_sfc
#' @importFrom stats approx
#' @importFrom stats complete.cases
#' @importFrom stats density
#' @importFrom stats dweibull
#' @importFrom stats lag
#' @importFrom stats line
#' @importFrom stats na.omit
#' @importFrom stats pexp
#' @importFrom stats plnorm
#' @importFrom stats pnorm
#' @importFrom stats pweibull
#' @importFrom stats quantile
#' @importFrom stats rbinom
#' @importFrom stats rexp
#' @importFrom stats rlnorm
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats rweibull
#' @importFrom stats sd
#' @importFrom stats setNames
#' @importFrom stats time
#' @importFrom utils data
#' @importFrom utils head
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".", "X", "Y",
  "age_seconds", "competency", "competency_point", "count",
  "decay_value", "direction", "dispersaltime", "distance",
  "endpoint", "geometry", "geometry_lag", "geometry_lagged",
  "habitat_id", "i", "id", "id_full", "id_prev", "id_row",
  "inst", "interval", "is_stationary", "land_retry_n",
  "lat", "linestring", "lon", "long_repeats",
  "lower90", "magnitude", "maxdispersaltime", "means",
  "minutes", "mortalitytime", "outcome", "output",
  "particle_i", "particle_id", "particle_points_expanded_postmortality",
  "particle_uid", "pid", "proportion_stationary",
  "release_i", "release_time", "release_x", "release_y",
  "se", "seascape", "settlement_outcome", "settlement_point",
  "settlement_probability", "settlement_time",
  "sim", "site_id", "speed", "speed_cm_s", "state", "status",
  "sum_time", "time_diff", "time_flow", "time_lag", "timecont",
  "total_moving_time", "total_stationary_time", "track_step", "type",
  "u_ms", "upper90", "v_ms", "velocity_e_cm_s", "velocity_n_cm_s",
  "x", "x_old", "x_prev", "y", "y_old"
))
