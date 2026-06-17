#' Simulate interpolated flow fields from tiltmeter velocity data
#'
#' Interpolates eastward and northward velocity components from tiltmeter
#' observations onto a regular spatial grid for each time step. The function
#' aggregates tiltmeter observations to a user-defined temporal resolution,
#' converts velocities from cm/s to m/s, creates a buffered spatial domain
#' around the instruments, and uses inverse distance weighting to generate
#' gridded flow fields.
#'
#' @param tiltmeters An `sf` object or data frame containing tiltmeter
#'   observations. Must contain columns `time`, `inst`, `lat`, `lon`,
#'   `velocity_e_cm_s`, `velocity_n_cm_s`, and `speed_cm_s`.
#' @param gbr_buffer An `sf` polygon object defining the spatial mask or reef
#'   buffer used to constrain the interpolation domain.
#' @param crs_m Numeric or character. Projected coordinate reference system
#'   used for interpolation in metres. Default is `32755`.
#' @param grid_res_m Numeric. Grid cell spacing in metres. Default is `10`.
#' @param buffer_m Numeric. Buffer distance in metres around the convex hull of
#'   tiltmeter locations. Default is `250`.
#' @param time_unit Character. Temporal aggregation unit passed to
#'   `lubridate::floor_date()`. Default is `"5 minutes"`.
#' @param idp Numeric. Inverse distance weighting power passed to
#'   `gstat::idw()`. Higher values give more weight to nearby observations.
#'   Default is `2`.
#'
#' @return A named list with:
#' \describe{
#'   \item{flow_field}{An `sf` point object in `crs_m` containing interpolated
#'   `u_ms`, `v_ms`, `speed_ms`, and `heading_degrees` for each grid cell and
#'   time step.}
#'   \item{flow_field_4326}{The same interpolated flow field transformed to
#'   EPSG:4326.}
#'   \item{tiltmeters_m}{Aggregated tiltmeter observations transformed to
#'   `crs_m`.}
#'   \item{grid_sf}{The interpolation grid as an `sf` point object in `crs_m`.}
#'   \item{domain_m}{The buffered interpolation domain clipped to
#'   `gbr_buffer`.}
#' }
#'
#' @details
#' The function first aggregates tiltmeter measurements by instrument and
#' floored time interval. Eastward and northward velocities are converted from
#' cm/s to m/s. A convex hull is constructed around the tiltmeter locations,
#' buffered by `buffer_m`, and clipped to `gbr_buffer`. A regular point grid is
#' then generated over this domain.
#'
#' For each time step, inverse distance weighting is applied separately to the
#' eastward (`u_ms`) and northward (`v_ms`) velocity components. Wind/current
#' speed is then calculated as:
#'
#' \deqn{speed = \sqrt{u^2 + v^2}}
#'
#' Heading is calculated in degrees using:
#'
#' \deqn{heading = atan2(u, v)}
#'
#' and converted to compass-style degrees from 0 to 360.
#'
#' Time steps with fewer than three tiltmeter observations are skipped.
#'
#' @examples
#' \dontrun{
#' flow_sim <- simulate_flowfields(
#'   tiltmeters = tiltmeters,
#'   gbr_buffer = gbr_buffer,
#'   crs_m = 32755,
#'   grid_res_m = 10,
#'   buffer_m = 250,
#'   time_unit = "5 minutes",
#'   idp = 2
#' )
#'
#' flow_sim$flow_field
#' flow_sim$flow_field_4326
#' }
#'
#' @importFrom dplyr mutate group_by summarise filter arrange
#' @importFrom lubridate floor_date
#' @importFrom purrr map_dfr
#' @importFrom sf st_drop_geometry st_as_sf st_transform st_union st_convex_hull
#'   st_buffer st_crop st_make_grid st_sf st_filter st_coordinates
#' @importFrom gstat idw
#' @importFrom tibble tibble
#'
#' @export
simulate_flowfields <- function(
    tiltmeters,
    gbr_buffer,
    crs_m = 32755,
    grid_res_m = 10,
    buffer_m = 250,
    time_unit = "5 minutes",
    idp = 2
) {

  requireNamespace("lubridate")
  requireNamespace("gstat")
  requireNamespace("sp")

  tiltmeters_m <- tiltmeters |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      time_flow = lubridate::floor_date(time, unit = time_unit),
      u_ms = velocity_e_cm_s / 100,
      v_ms = velocity_n_cm_s / 100
    ) |>
    dplyr::group_by(inst, lat, lon, time_flow) |>
    dplyr::summarise(
      u_ms = mean(u_ms, na.rm = TRUE),
      v_ms = mean(v_ms, na.rm = TRUE),
      speed_ms = mean(speed_cm_s / 100, na.rm = TRUE),
      .groups = "drop"
    ) |>
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    ) |>
    sf::st_transform(crs_m)

  domain_m <- tiltmeters_m |>
    sf::st_union() |>
    sf::st_convex_hull() |>
    sf::st_buffer(buffer_m) |>
    sf::st_crop(sf::st_transform(gbr_buffer, crs_m))

  grid_geom <- sf::st_make_grid(
    domain_m,
    cellsize = grid_res_m,
    what = "centers"
  )

  grid_sf <- sf::st_sf(
    grid_id = seq_along(grid_geom),
    geometry = grid_geom,
    crs = crs_m
  ) |>
    sf::st_filter(domain_m)

  grid_xy <- sf::st_coordinates(grid_sf)

  grid_sf <- grid_sf |>
    dplyr::mutate(
      x = grid_xy[, 1],
      y = grid_xy[, 2]
    )

  sf::st_geometry(grid_sf) <- "geometry"

  grid_sp <- as(grid_sf, "Spatial")

  interpolate_one_time <- function(dat_t, grid_sf, grid_sp, idp = 2) {

    if (nrow(dat_t) < 3) {
      return(tibble::tibble())
    }

    dat_xy <- sf::st_coordinates(dat_t)

    dat_t <- dat_t |>
      dplyr::mutate(
        x = dat_xy[, 1],
        y = dat_xy[, 2]
      )

    sf::st_geometry(dat_t) <- "geometry"

    dat_sp <- as(dat_t, "Spatial")

    u_idw <- gstat::idw(
      formula = u_ms ~ 1,
      locations = dat_sp,
      newdata = grid_sp,
      idp = idp,
      debug.level = 0
    )

    v_idw <- gstat::idw(
      formula = v_ms ~ 1,
      locations = dat_sp,
      newdata = grid_sp,
      idp = idp,
      debug.level = 0
    )

    grid_sf |>
      sf::st_drop_geometry() |>
      dplyr::mutate(
        time = unique(dat_t$time_flow),
        u_ms = u_idw$var1.pred,
        v_ms = v_idw$var1.pred,
        speed_ms = sqrt(u_ms^2 + v_ms^2),
        heading_degrees = (atan2(u_ms, v_ms) * 180 / pi + 360) %% 360
      )
  }

  flow_field <- tiltmeters_m |>
    dplyr::group_split(time_flow) |>
    purrr::map_dfr(
      interpolate_one_time,
      grid_sf = grid_sf,
      grid_sp = grid_sp,
      idp = idp
    ) |>
    sf::st_as_sf(
      coords = c("x", "y"),
      crs = crs_m,
      remove = FALSE
    )

  list(
    flow_field = flow_field,
    flow_field_4326 = sf::st_transform(flow_field, 4326),
    tiltmeters_m = tiltmeters_m,
    grid_sf = grid_sf,
    domain_m = domain_m
  )
}
