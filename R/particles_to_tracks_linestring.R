#' Particles to tracks
#'
#' Function to convert particles (sf points) to tracks (sf linestrings, or here a multilinestring)
#'
#' note: GEOS throws an error when n points is less than 3. When using the by argument the function drops
#' levels with less than 3. This isn't an issue for mapping as only removes late competency particles, but
#' check and be careful converting other factors to paths:
#' https://gis.stackexchange.com/questions/447578/geosexception-illegalargumentexception-point-array-must-contain-0-or-1-elemen
#'
#' !!sym(by)
#'
#' @param input input (defaults to NULL)
#' @param by factor level
#' @param slicesample subset to n particles

#'
#' @export
#'
#'

particles_to_tracks_linestring <- function(input = NULL, slicesample=100, by="competency") {
  options(dplyr.summarise.inform = FALSE)

  idstring <- sample(unique(input$id), slicesample)

  tracks <- input %>%
    dplyr::arrange(id, dispersaltime) %>%
    dplyr::group_by(id, competency) %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_make_valid() %>%
    sf::st_cast("LINESTRING")

  tracks_filtered <- tracks[sapply(st_geometry(tracks), st_is_valid), ]

  tracks_filtered <- tracks_filtered %>% filter(id %in% idstring)

  options(dplyr.summarise.inform = TRUE)

  return(tracks_filtered)
}
