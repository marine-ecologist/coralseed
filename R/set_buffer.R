#' Set buffer
#'
#' Function to generate an sf file for bounding box. Used only for tmap visualisations
#'
#' @param input input (defaults to NULL)
#' @param buffer distance surrounding particle boundary (defaults to 250m)
#' @export
#'
#'

set_buffer <- function(input=NULL, buffer=150){
  outer_limits <- input |>
    sf::st_transform("EPSG:20353") |> # at a 500m buffer around spatial points
    sf::st_union() |>
    sf::st_convex_hull() |>
    sf::st_buffer(buffer) |>
    sf::st_bbox() |>
    sf::st_as_sfc()
  return(outer_limits)
}
