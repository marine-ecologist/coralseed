#' Particles to tracks
#'
#' Function to convert particles (sf points) to tracks (sf linestrings, or here a multilinestring)
#'
#' note: GEOS throws an error when n points is less than 3. When using the by argument the function drops
#' levels with less than 3. This isn't an issue for mapping as only removes late competency particles, but
#' check and be careful converting other factors to paths:
#' https://gis.stackexchange.com/questions/447578/geosexception-illegalargumentexception-point-array-must-contain-0-or-1-elemen
#'
#' @param input input (defaults to NULL)
#' @export
#'
#'

particles_to_tracks <- function(input = NULL) {
  options(dplyr.summarise.inform = FALSE)
  tracks <- input |>
    dplyr::arrange(id, time) |>
    #  dplyr::group_by(id,{{by}}) |>
    dplyr::group_by(id, competency) |>
    dplyr::filter(dplyr::n() >= 3) |>
    dplyr::group_by(id, competency) |>
    dplyr::summarise(do_union = FALSE) |>
    sf::st_cast("MULTILINESTRING")
  options(dplyr.summarise.inform = TRUE)
  return(tracks)
}
