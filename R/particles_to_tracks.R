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

particles_to_tracks <- function(input = NULL, slicesample=100, by="competency") {
  options(dplyr.summarise.inform = FALSE)
  
  tracks <- input %>% 
    # remove duplicate geometries if particle is static or breaks linestring
    #group_by(geometry) %>% 
    #slice_head(n = 1) %>% 
    #ungroup() %>%
   
     #mutate(id=as.factor(id)) |> 
    #mutate(competency=as.factor(competency)) |> 
    arrange(id, dispersaltime) %>% 
    group_by(id, competency) %>%
    summarise(do_union = FALSE) %>% 
    st_make_valid() %>%
    st_cast("MULTILINESTRING") |> 
    slice_sample(n=slicesample)
  
  
  options(dplyr.summarise.inform = TRUE)
  
  return(tracks)
}
