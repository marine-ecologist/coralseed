#' Set restoration plot
#'
#' Function to create a rectangular polygon around the release point (centroid or t0 of particle release) for plotting in tmap
#'
#' @param input input (defaults to NULL)
#' @param width width in metres
#' @param length length in metres
#' @param center not yet defined, but later to change centre point
#' @export
#'


set_restoration_plot <- function(input = NULL, width = NULL, length = NULL, center = NULL) {
  if (is.null(center)) {
    tmp_min_centroid <- input |>
      dplyr::filter(dispersaltime == 0) |>
      dplyr::summarize(geometry = sf::st_union(geometry))
  } else {
    print("Add option to set centroid later")
  }

  # Calculate the coordinates of the rectangular polygon
  x <- sf::st_coordinates(tmp_min_centroid)[1, 1]
  y <- sf::st_coordinates(tmp_min_centroid)[1, 2]

  # set parameters
  x_min <- x - (width / 2)
  x_max <- x + (width / 2)
  y_min <- y - (length / 2)
  y_max <- y + (length / 2)

  polygon <- sf::st_polygon(list(rbind(c(x_min, y_min), c(x_min, y_max), c(x_max, y_max), c(x_max, y_min), c(x_min, y_min)))) |>
    sf::st_sfc(crs = 20353)

  return(polygon)
}

