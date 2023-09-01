#' Particle distances
#'
#' Function to calculate distances of particles at any timepoint
#' limit the particle distances to tmax (in minutes, e.g. 60 for 1hr)
#' returns either  "sf" ("MULTILINGSTRING" for each id) or "df" (data.frame with total length for each id) depending on type="df" or type="sf"
#'
#' @param input input (defaults to NULL)
#' @param tmax limit particle times to less than tmax
#' @param type export format to "sf" or "df" (see above)
#' @param ... pass arguments
#' @export
#'

particle_distances <- function(input = NULL, tmax = NULL, type = "df") {
  # options(dplyr.summarise.inform = FALSE)
  if (type == "sf") {
    lengths <- input |>
      dplyr::filter(dispersaltime < tmax) |>
      dplyr::group_by(id) |>
      dplyr::summarise(do_union = FALSE) |>
      sf::st_cast("MULTILINESTRING") |>
      dplyr::group_by(id) |>
      dplyr::summarise(distance = sf::st_length(geometry))
    # options(dplyr.summarise.inform = TRUE)

    return(lengths)
  } else if (type == "df") {
    lengths <- input |>
      dplyr::filter(dispersaltime < tmax) |>
      dplyr::group_by(id) |>
      dplyr::summarise(do_union = FALSE) |>
      sf::st_cast("MULTILINESTRING") |>
      dplyr::group_by(id) |>
      dplyr::summarise(distance = sf::st_length(geometry)) |>
      as.data.frame() |>
      dplyr::select(id, distance)
    colnames(lengths) <- c("id", paste0("t", tmax))
    return(lengths)
  }
}
