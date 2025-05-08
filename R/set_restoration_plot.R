#' Set restoration plot
#'
#' Function to create a rectangular polygon around the release point (centroid or t0 of particle release) for plotting in tmap
#'
#' @param input input `sf` object used to derive the center
#' @param width width in metres
#' @param length length in metres
#' @param crs CRS for output object (default 4326)
#' @param center either "coralseed" for t0-based centroid, or "sf" for direct centroid
#' @export
#'

set_restoration_plot <- function(input = NULL, width = NULL, length = NULL, crs = 4326, center = "coralseed") {
  s2_state <- sf_use_s2()
  warn_state <- getOption("warn")
  options(sf_use_s2 = FALSE)
  options(warn = -1)

  if (center == "coralseed") {
    tmp_min_centroid <- input |>
      dplyr::filter(dispersaltime == 0) |>
      dplyr::summarize(geometry = sf::st_union(geometry))

    x <- sf::st_coordinates(tmp_min_centroid)[1, 1]
    y <- sf::st_coordinates(tmp_min_centroid)[1, 2]

    x_min <- x - (width / 2)
    x_max <- x + (width / 2)
    y_min <- y - (length / 2)
    y_max <- y + (length / 2)

    polygon <- sf::st_polygon(list(rbind(
      c(x_min, y_min), c(x_min, y_max), c(x_max, y_max),
      c(x_max, y_min), c(x_min, y_min)
    ))) |>
      sf::st_sfc(crs = 20353) |>
      sf::st_transform(crs = crs)

  } else if (center == "sf") {
    region_centroid <- sf::st_centroid(input)
    region_centroid_proj <- sf::st_transform(region_centroid, crs = 32755)

    half_width <- width / 2
    half_length <- length / 2

    bbox_coords <- sf::st_coordinates(region_centroid_proj)[1, ]
    bounding_box_proj <- sf::st_polygon(list(rbind(
      c(bbox_coords[1] - half_width, bbox_coords[2] - half_length),
      c(bbox_coords[1] + half_width, bbox_coords[2] - half_length),
      c(bbox_coords[1] + half_width, bbox_coords[2] + half_length),
      c(bbox_coords[1] - half_width, bbox_coords[2] + half_length),
      c(bbox_coords[1] - half_width, bbox_coords[2] - half_length)
    ))) |>
      sf::st_sfc(crs = 32755) |>
      sf::st_transform(crs = crs)

    polygon <- bounding_box_proj
  }

  options(sf_use_s2 = s2_state)
  options(warn = warn_state)
  return(polygon)
}
#
# set_restoration_plot <- function(input = NULL, width = NULL, length = NULL, crs=4326, center = "coralseed") {
#   s2_state <- sf_use_s2()
#   warn_state <- getOption("warn")
#   options(sf_use_s2 = FALSE)  # Disable spherical geometry if needed
#   options(warn = -1)          # Suppress all warnings globally
#
#   if (center== "coralseed") {
#     tmp_min_centroid <- input |>
#       dplyr::filter(dispersaltime == 0) |>
#       dplyr::summarize(geometry = sf::st_union(geometry))
#
#     # Calculate the coordinates of the rectangular polygon
#     x <- sf::st_coordinates(tmp_min_centroid)[1, 1]
#     y <- sf::st_coordinates(tmp_min_centroid)[1, 2]
#
#     # set parameters
#     x_min <- x - (width / 2)
#     x_max <- x + (width / 2)
#     y_min <- y - (length / 2)
#     y_max <- y + (length / 2)
#
#     polygon <- sf::st_polygon(list(rbind(c(x_min, y_min), c(x_min, y_max), c(x_max, y_max), c(x_max, y_min), c(x_min, y_min)))) |>
#       sf::st_sfc(crs = 20353)
#
#
#   } else if (center== "sf"){
#     # Calculate the centroid
#     region_centroid <- st_centroid(input)
#
#     # Transform the centroid to a projected CRS for accurate distance calculations
#     region_centroid_proj <- st_transform(region_centroid, crs = 32755)
#
#     # Create a buffer to define the box dimensions (width and length in meters)
#     half_width <- width / 2
#     half_length <- length / 2
#
#     # Manually create a rectangular bounding box around the centroid
#     bbox_coords <- st_coordinates(region_centroid_proj)[1, ]
#     bounding_box_proj <- st_polygon(list(rbind(
#       c(bbox_coords[1] - half_width, bbox_coords[2] - half_length),
#       c(bbox_coords[1] + half_width, bbox_coords[2] - half_length),
#       c(bbox_coords[1] + half_width, bbox_coords[2] + half_length),
#       c(bbox_coords[1] - half_width, bbox_coords[2] + half_length),
#       c(bbox_coords[1] - half_width, bbox_coords[2] - half_length)  # Close the polygon
#     )))
#
#     # Convert to an sfc object and set CRS
#     bounding_box_proj <- st_sfc(bounding_box_proj, crs = 32755)
#
#     # Transform the bounding box back to WGS84
#     bounding_box_wgs84 <- st_transform(bounding_box_proj, crs = 4326)
#
#     return(bounding_box_wgs84)
#
#   }
#
#
#   options(sf_use_s2 = s2_state)  # Disable spherical geometry if needed
#   options(warn = warn_state)          # Suppress all warnings globally
#
#   return(polygon)
# }
#
