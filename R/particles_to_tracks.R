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
#' 
#' @export
#'
#'

particles_to_tracks <- function(input = NULL,  by="id", type="MULTILINESTRING") {
  options(dplyr.summarise.inform = FALSE)
  
  
  tracks <- input %>% 
    #remove duplicate geometries if particle is static or breaks linestring
    group_by(geometry) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    #drop less than 3 points per group
    group_by(id, competency) |> 
    filter(n() > 3 ) |> 
    #mutate(id=as.factor(id)) |> 
    #mutate(competency=as.factor(competency)) |> 
    dplyr::arrange(id, dispersaltime) %>% 
    dplyr::group_by(!!!syms(by)) %>%
    dplyr::summarise(do_union = FALSE) %>% 
    sf::st_make_valid() %>%
    sf::st_cast(type) %>%
    # dplyr::ungroup() %>%
    # dplyr::mutate(id=as.factor(id)) %>%
    # dplyr::group_by(id) %>%
    # dplyr::sample_n(slicesample) %>%
    # dplyr::mutate(id=as.character(id))
    
  
  #options(dplyr.summarise.inform = TRUE)
  
  return(tracks)
}
