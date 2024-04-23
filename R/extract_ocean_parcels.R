
#' extract_ocean_particles
#'
#' Function to convert ocean particles Zarr folders to coralseed format
#' Extracts metadata to get correct time format
#' uses Rarr:read_zarr_array() to get lat, lon, time, z arrays
#' converts to sf points
#'
#' Example usage
#' folder <- "/Users/rof011/Mooreoutputs2015Moore_trajectory_2015_d12d.zarr_mon20th.zarr/"
#' sf_data <- extract_ocean_parcels(folder, sample=10, crs=20353)
#' print(sf_data)
#'
#'
#' @param folder input folder e.g. /outputs.zarr/
#' @param crs output crs for sf object
#' @param subsample sample n from total oceanparcels trajectories
#' @param depth TRUE/FALSE, if FALSE returns 2D array (xy), if TRUE returns 3D array (xyz)
#' @export
#'



extract_ocean_parcels <- function(folder, crs = 4326, subsample = "none", depth=FALSE) {

  # read metadata for time origin
  zmetadata_path <- paste0(folder, ".zmetadata")
  zmetadata_content <- jsonlite::fromJSON(zmetadata_path)
  time_zattrs <- zmetadata_content$metadata$`time/.zattrs`
  units_str <- time_zattrs$units
  origin_datetime <- sub("seconds since ([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}).*", "\\1", units_str)

  #print(origin_datetime)

  # Load arrays
  lat_array <- Rarr::read_zarr_array(paste0(folder, "lat"))
  lon_array <- Rarr::read_zarr_array(paste0(folder, "lon"))
  z_array <- Rarr::read_zarr_array(paste0(folder, "z"))
  time_array <- Rarr::read_zarr_array(paste0(folder, "time"))
  obs_array <- Rarr::read_zarr_array(paste0(folder, "obs"))
  trajectory_array <- Rarr::read_zarr_array(paste0(folder, "trajectory"))

  # Determine the number of trajectories to process
  num_trajectories <- dim(trajectory_array)[1]

  # subsample trajectories if subsample is numeric
  if (is.numeric(subsample)) {
    set.seed(123)  # Ensure reproducibility
    sampled_indices <- sample(1:num_trajectories, subsample, replace = FALSE)
  } else {
    sampled_indices <- 1:num_trajectories
  }

  # Function to create a data frame for each trajectory
  create_trajectory_df <- function(i) {
    df <- data.frame(
      lon = lon_array[i, ],
      lat = lat_array[i, ],
      z = z_array[i, ],
      time = as.POSIXct(time_array[i, ], origin=origin_datetime, tz="UTC"),
      obs = obs_array
    )

    # Remove rows with NA values (if any)
    df_clean <- df[complete.cases(df), ]

    # Add trajectory ID
    df_clean$id <- trajectory_array[i]

    return(df_clean)
  }

  # Use purrr::map_dfr to apply the function over sampled trajectories and combine results
  df_combined <- purrr::map_dfr(sampled_indices, create_trajectory_df)

  # Convert the combined data frame to an sf object
  sf_combined <- sf::st_as_sf(df_combined, coords = c("lon", "lat", "z"), crs = 4326) |>
   # rename(id=trajectory) |>
    select(id, time)

  # Transform to the specified CRS
  sf_transformed <- sf::st_transform(sf_combined, crs = crs)

  # drop Z axis for 2D arrays
  if (isFALSE(depth)) {
    sf_transformed <- sf::st_zm(sf_transformed, drop = TRUE)
  }

  # Return the transformed sf object
  return(sf_transformed)
}

