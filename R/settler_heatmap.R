#' Generate a Heatmap of Settler Particles
#'
#' This function computes a kernel density estimate (KDE) heatmap from spatial
#' points (settler particles) and returns a raster representation. The bounding
#' box is expanded by a specified factor to avoid edge effects.
#'
#' @param input An sf object containing point geometries representing settler particles.
#' @param xres Numeric. The resolution of the heatmap in the x direction meters. Default is 20.
#' @param yres Numeric. The resolution of the heatmap in the y direction meters. Default is 20.
#' @param buffer_factor Numeric. The proportion of the bounding box range to expand the heatmap extent. Default is 0.1 10%.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(terra)
#'
#' # Example: Generate a random set of points
#' set.seed(123)
#' points_sf <- st_as_sf(data.frame(
#'   X = runif(100, min = 1000, max = 2000),
#'   Y = runif(100, min = 500, max = 1500)
#' ), coords = c("X", "Y"), crs = 4326)
#'
#' # Generate heatmap with 50m resolution and 10% buffer
#' heatmap <- settler_heatmap(points_sf, xres = 50, yres = 50, buffer_factor = 0.1)
#'
#' # Plot result
#' plot(heatmap)
#' }

settler_heatmap <- function(input, xres = 20, yres = 20, buffer_factor = 0.1) {

  # Extract points from sf object
  points <- input

  # Convert sf points to a data frame with coordinates
  particle_points_df <- sf::st_coordinates(points) |>
    as.data.frame() |>
    dplyr::rename(X = X, Y = Y)

  # Get bounding box for kde2d limits
  particle_bbox <- sf::st_bbox(points)

  # Handle cases where min/max are equal (zero range)
  eps <- 1e-6  # Small padding to prevent errors
  if (particle_bbox["xmin"] == particle_bbox["xmax"]) {
    particle_bbox["xmin"] <- particle_bbox["xmin"] - eps
    particle_bbox["xmax"] <- particle_bbox["xmax"] + eps
  }
  if (particle_bbox["ymin"] == particle_bbox["ymax"]) {
    particle_bbox["ymin"] <- particle_bbox["ymin"] - eps
    particle_bbox["ymax"] <- particle_bbox["ymax"] + eps
  }

  # Compute buffer expansion
  buffer_x <- buffer_factor * (particle_bbox["xmax"] - particle_bbox["xmin"])
  buffer_y <- buffer_factor * (particle_bbox["ymax"] - particle_bbox["ymin"])

  # Ensure limits are finite and valid
  expanded_bbox <- c(
    xmin = max(particle_bbox["xmin"] - buffer_x, -1e9),
    xmax = min(particle_bbox["xmax"] + buffer_x, 1e9),
    ymin = max(particle_bbox["ymin"] - buffer_y, -1e9),
    ymax = min(particle_bbox["ymax"] + buffer_y, 1e9)
  )

  # Compute 2D Kernel Density Estimation
  kde_result <- MASS::kde2d(
    particle_points_df$X, particle_points_df$Y,
    n = 100,  # Adjust KDE grid resolution
    lims = c(expanded_bbox["xmin"], expanded_bbox["xmax"],
             expanded_bbox["ymin"], expanded_bbox["ymax"])
  )

  # Define new raster grid with correct expanded resolution
  new_r <- terra::rast(
    xmin = expanded_bbox["xmin"], xmax = expanded_bbox["xmax"],
    ymin = expanded_bbox["ymin"], ymax = expanded_bbox["ymax"],
    res = c(xres, yres),
    crs = sf::st_crs(points)  # Set CRS to match points
  )

  # Convert kde2d density estimate to a terra raster
  r <- terra::rast(
    nrows = length(kde_result$y), ncols = length(kde_result$x),
    xmin = min(kde_result$x), xmax = max(kde_result$x),
    ymin = min(kde_result$y), ymax = max(kde_result$y)
  )

  terra::values(r) <- as.vector(kde_result$z)

  # Resample raster to match expanded grid
  heatmap <- terra::resample(r, new_r, method = "bilinear")

  # Set CRS using terra
  terra::crs(heatmap) <- terra::crs(points)

  # Apply thresholding to remove low-density values
  max_value <- max(terra::values(heatmap), na.rm = TRUE)
  threshold <- max_value / 10
  heatmap[heatmap < threshold] <- NA

  # Flip the raster for correct orientation
#  heatmap <- terra::flip(heatmap)

  return(heatmap)
}

