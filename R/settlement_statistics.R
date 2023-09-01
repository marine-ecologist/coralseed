#' Settlement statistics
#'
#' Function to generate settlement statistics from coralseed output
#'
#' @param input input (defaults to NULL)
#' @param combined test to allow multiple coralseed file inputs in list (defaults to FALSE)
#' @param cellsize dimensions of grid to count settlers (defaults to 20m)
#' @param concavehull return concave hull around settled particles (via concaveman)
#' @param concavity concavity value passed to concaveman (defaults to 2
#' @param length_threshold length threshold passed to concaveman (defaults to 0)
#' @param ... pass arguments
#' @export
#'
settlement_statistics <- function(
    input = NULL, combined = FALSE, cellsize = 20, concavehull = TRUE,
    concavity = 2, length_threshold = 0, ...) {


  # make a spatial grid across the settled particles, set cell size (metres)
  settled_particles_grid <- input |>
    dplyr::select(-id, -class, -time, -cat) |>
    sf::st_make_grid(cellsize = cellsize, what = "polygons") # 20metre grid

  # calculate the density of larvae within each grid-cell, convert to sf
  grid_count <- sp::over(sf::as_Spatial(settled_particles_grid), sf::as_Spatial(select(input, id, class)), fn = length)
  settled_particles_count <- sf::st_as_sf(settled_particles_grid) |>
    dplyr::mutate(count = as.numeric(tidyr::replace_na(grid_count$id, NA)))
  settled_particles_density <- sf::st_as_sf(settled_particles_count) |>
    dplyr::mutate(density = count / (cellsize * cellsize))


  concavehull <- concaveman::concaveman(input, concavity = 2, length_threshold = 0)

  settlement_output <- list(concavehull, settled_particles_density)

  names(settlement_output) <- c("area", "grid")

  return(settlement_output)
}
