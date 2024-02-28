#' Settlement density
#'
#' Function to calculate settlement density across a grid
#' Uses sp::over instead of sf for speed
#'
#' @param input input (defaults to NULL)
#' @param calculate_hull true/false calculate concave hull (can be time intensive on large datasets, defaults to true)
#' @param combined test to allow multiple coralseed file inputs in list (defaults to FALSE)
#' @param cellsize dimensions of grid to count settlers (defaults to 20m)
#' @param concavity concavity value passed to concaveman (defaults to 2
#' @param length_threshold length threshold passed to concaveman (defaults to 0)
#' @param ... pass arguments
#' @export
#'

settlement_density <- function(input = NULL, calculate_hull=TRUE, combined=FALSE, cellsize = 20, concavity = 1.2, length_threshold = 10, ...) {
  ##### function to calculate density
  if (combined==TRUE){
    input #|> dplyr::ungroup()
  }else if (combined==FALSE){
    input <- input |> with(points) # |> dplyr::ungroup()
  }
  # make a spatial grid across the settled particles, set cell size (metres)
  settled_particles_grid <- input |>
    dplyr::select(-id, -class, -time, -cat) |>
    sf::st_make_grid(cellsize = cellsize, what = "polygons") # 20metre grid

  # calculate the density of larvae within each grid-cell, convert to sf
  grid_count <- sp::over(sf::as_Spatial(settled_particles_grid), sf::as_Spatial(dplyr::select(input, id, class)), fn = length)

  settled_particles_count <- sf::st_as_sf(settled_particles_grid) |>
    dplyr::mutate(count = as.numeric(tidyr::replace_na(grid_count$id, NA)))

  settled_particles_density <- settled_particles_count |>
      dplyr::mutate(density = count / (cellsize * cellsize))

  if (calculate_hull==TRUE){
    settled_particles_concavehull <- concaveman::concaveman(input, concavity = concavity, length_threshold = length_threshold)
    settled_particles_concavehull$area <- round(sf::st_area(settled_particles_concavehull))

    set_out <- list(settled_particles_count, settled_particles_density, settled_particles_concavehull)
    names(set_out) <- c("count", "density", "area")

  } else {

    set_out <- list(settled_particles_count, settled_particles_density)
    names(set_out) <- c("count", "density")

  }

  return(set_out)
}
