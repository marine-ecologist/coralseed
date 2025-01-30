#' Seed particles
#'
#' Function to convert xarrr arrays from oceanparticles to df format
#'
#' zarr_file_path <- "/Users/rof011/oceanparcels/outputs/Elford_16073_Slope_71a.zarr/"
#'
#'
#' @param input input .zarr folder
#' @export
#'

import_zarr <- function(input) {

  # Correct the paths for each variable
  lon <- Rarr::read_zarr_array(zarr_array_path = paste0(input, "lon/"))
  lat <- Rarr::read_zarr_array(zarr_array_path = paste0(input, "lat/"))
  time <- Rarr::read_zarr_array(zarr_array_path = paste0(input, "time/"))
  trajectory <- Rarr::read_zarr_array(zarr_array_path = paste0(input, "trajectory/"))
  obs <- Rarr::read_zarr_array(zarr_array_path = paste0(input, "obs/"))

  # Flatten the arrays (convert matrices to vectors)
  lat_vec <- as.vector(lat)
  lon_vec <- as.vector(lon)
  time_vec <- as.vector(time)
  trajectory_vec <- rep(trajectory, times = ncol(lat))  # Replicate each trajectory across columns
  obs_vec <- rep(obs, each = nrow(lat))                 # Repeat each obs value across rows

  # Convert the time to POSIXct
  time_vec <- as.POSIXct(time_vec, origin = "1970-01-01", tz = "UTC")

  # Create a data frame
  data_df <- data.frame(
    lon = lon_vec,
    lat = lat_vec,
    time = time_vec,
    obs = obs_vec,
    trajectory = trajectory_vec
  ) %>%
    dplyr::rename(id = obs) |>
    dplyr::select(-trajectory) |>
    dplyr::filter(!is.na(lon), !is.na(lat)) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  return(data_df)
}
