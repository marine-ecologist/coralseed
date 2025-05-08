#' plot_recom: Visualize Ocean Currents and Wind from NetCDF Data
#'
#' Reads a NetCDF file containing ocean current and wind data, extracts relevant
#' variables at a specified depth (-2.35m), and generates time-series and spatial plots.
#' Highlights slack current time and visualizes magnitude and direction of currents and wind.
#'
#' @param nc_file Character. Path to the NetCDF file containing oceanographic data.
#' @param parcels_reefs sf object. Spatial dataset of reef locations.
#' @param parcels_polygon sf object. Spatial dataset for a specific study site polygon.
#'
#' @return A combined ggplot object displaying temporal trends of ocean currents and wind,
#'         along with spatial maps of current speeds at the slack current time.
#'
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom terra rast crop time
#' @importFrom sf st_buffer
#' @importFrom dplyr mutate slice_min pull
#' @importFrom ggplot2 ggplot geom_line geom_segment geom_hline geom_vline theme_bw aes scale_fill_distiller
#' @importFrom patchwork wrap_plots
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom lubridate hours
#' @importFrom tidyterra geom_spatraster
#' @importFrom grid arrow unit
#'
#' @export
#'
plot_recom <- function(nc_file = "/Users/rof011/GBR_connectivity/Oceanparcels/Moore_uv_2015-12-05.nc",
                       parcels_reefs = sf::st_read("/Users/rof011/GBR_connectivity/inputs/Moore_2D.gpkg", quiet=TRUE),
                       parcels_polygon = sf::st_read("/Users/rof011/GBR_connectivity/inputs/Moore_2D.gpkg", quiet=TRUE) |> dplyr::filter(site_id == "Moore_16071_Slope_39a")) {
  # Open the NetCDF file
  nc <- ncdf4::nc_open(nc_file)
  lon <- ncdf4::ncvar_get(nc, "longitude") # 56x56 grid
  lat <- ncdf4::ncvar_get(nc, "latitude")  # 56x56 grid
  time <- ncdf4::ncvar_get(nc, "time")  # Time in days since 1990-01-01
  depths <- ncdf4::ncvar_get(nc, "zc")  # Depth levels
  u <- ncdf4::ncvar_get(nc, "v")  # Eastward ocean current
  v <- ncdf4::ncvar_get(nc, "w")  # Northward ocean current
  wspeed_u <- ncdf4::ncvar_get(nc, "wspeed_u")  # Eastward wind
  wspeed_v <- ncdf4::ncvar_get(nc, "wspeed_v")  # Northward wind
  ncdf4::nc_close(nc)

  # Convert time to actual dates
  time_origin <- as.POSIXct("1990-01-01", tz = "UTC")
  time_values <- time_origin + time * 86400  # Convert days to seconds

  # Find depth index closest to -2.35m
  depth_idx <- which.min(abs(depths - (-2.35)))

  # Extract ocean current data at selected depth
  u_selected_depth <- u[, , depth_idx, ]
  v_selected_depth <- v[, , depth_idx, ]

  # Convert to raster format for ocean currents
  r_u <- terra::rast(array(u_selected_depth, dim = c(56, 56, length(time))),
                     extent = c(min(lon), max(lon), min(lat), max(lat)),
                     crs = "EPSG:4326")
  terra::time(r_u) <- time_values

  r_v <- terra::rast(array(v_selected_depth, dim = c(56, 56, length(time))),
                     extent = c(min(lon), max(lon), min(lat), max(lat)),
                     crs = "EPSG:4326")
  terra::time(r_v) <- time_values

  # Convert to raster format for wind speed
  r_wspeed_u <- terra::rast(array(wspeed_u, dim = c(56, 56, length(time))),
                            extent = c(min(lon), max(lon), min(lat), max(lat)),
                            crs = "EPSG:4326")
  terra::time(r_wspeed_u) <- time_values

  r_wspeed_v <- terra::rast(array(wspeed_v, dim = c(56, 56, length(time))),
                            extent = c(min(lon), max(lon), min(lat), max(lat)),
                            crs = "EPSG:4326")
  terra::time(r_wspeed_v) <- time_values

  # Crop raster to region of interest (parcels_polygon)
  u_time <- terra::crop(r_u, terra::vect(parcels_polygon))
  v_time <- terra::crop(r_v, terra::vect(parcels_polygon))
  wspeed_u_time <- terra::crop(r_wspeed_u, terra::vect(parcels_polygon))
  wspeed_v_time <- terra::crop(r_wspeed_v, terra::vect(parcels_polygon))

  # Convert to data frames for plotting
  currents_df <- data.frame(time = time_values,
                            u = as.numeric(terra::values(u_time)),
                            v = as.numeric(terra::values(v_time))) |>
    tidyr::pivot_longer(c(u, v), names_to = "direction", values_to = "speed")

  wind_df <- data.frame(time = time_values,
                        wspeed_u = as.numeric(terra::values(wspeed_u_time)),
                        wspeed_v = as.numeric(terra::values(wspeed_v_time))) |>
    tidyr::pivot_longer(c(wspeed_u, wspeed_v), names_to = "direction", values_to = "speed")

  # Compute magnitude and bearing for ocean currents
  currents_df <- currents_df |>
    dplyr::group_by(time, direction) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::ungroup()

  currents_wide <- currents_df |>
    tidyr::pivot_wider(names_from = direction, values_from = speed) |>
    dplyr::mutate(
      magnitude = sqrt(u^2 + v^2),
      bearing = atan2(v, u) * (180 / pi)
    )


  # Compute magnitude and bearing for wind
  wind_df <- wind_df |>
    dplyr::group_by(time, direction) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::ungroup()

  wind_wide <- wind_df |>
    tidyr::pivot_wider(names_from = direction, values_from = speed)  |>
    dplyr::mutate(
      magnitude = sqrt(wspeed_u^2 + wspeed_v^2),
      bearing = atan2(wspeed_v, wspeed_u) * (180 / pi)
    )

  # Find slack current time (minimum current magnitude)
  currents_slack <- currents_wide |>
    dplyr::slice_min(magnitude, n = 1) |>
    dplyr::pull(time) |>
    as.POSIXct()

  # Define arrow aesthetics
  arrow_style <- grid::arrow(length = grid::unit(3, "mm"), type = "closed")

  # Temporal plots (current and wind)
  temporal_currents <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::geom_line(data = currents_df, ggplot2::aes(time, speed, color = direction)) +
    ggplot2::geom_vline(xintercept = currents_slack, linetype = "dashed", color = "black") +
    ggplot2::geom_segment(data = currents_wide, linewidth = 0.4,
                          arrow = arrow_style,
                          ggplot2::aes(x = time, xend = time + 3600, y = 0, yend = u)) +
    ggplot2::ggtitle("Currents")

  temporal_wind <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::geom_line(data = wind_df, ggplot2::aes(time, speed, color = direction)) +
    ggplot2::geom_vline(xintercept = currents_slack, linetype = "dashed", color = "black") +
    ggplot2::geom_segment(data = wind_wide, linewidth = 0.4,
                          arrow = arrow_style,
                          ggplot2::aes(x = time, xend = time + 3600, y = 0, yend = wspeed_u)) +
    ggplot2::ggtitle("Wind")

  # Get the time index corresponding to slack current
  time_idx <- match(currents_slack, time_values)

  peak_u <- ggplot2::ggplot() + ggplot2::theme_bw() +
    tidyterra::geom_spatraster(data = r_u[[time_idx]], show.legend = FALSE) +
    ggplot2::geom_sf(data = parcels_reefs, fill = "black", alpha = 0.2) +
    ggplot2::geom_sf(data = parcels_polygon, fill = "red")

  peak_v <- ggplot2::ggplot() + ggplot2::theme_bw() +
    tidyterra::geom_spatraster(data = r_v[[time_idx]]) +
    ggplot2::geom_sf(data = parcels_reefs, fill = "black", alpha = 0.2) +
    ggplot2::geom_sf(data = parcels_polygon, fill = "red")

  return(patchwork::wrap_plots(temporal_wind, temporal_currents, peak_u, peak_v))
}
