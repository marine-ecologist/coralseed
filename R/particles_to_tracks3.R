#' Particles to tracks v3.0
#'
#' Function to convert particles (sf points) to tracks (sf linestrings, or here a multilinestring)
#' 
#' 
#' 
#' Avoids the issue below:
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

particles_to_tracks3 <- function(input = NULL, by="competency", multilinestring=TRUE) {
  options(dplyr.summarise.inform = FALSE)
 
  
    tmp2 <- input %>% 
      arrange(id) %>% # Ensure data is sorted by id for lag function
      group_by(id, competency) %>%
      mutate(
        geometry_lagged = lag(geometry, default =  st_as_sfc("POINT(EMPTY)", crs = 20353))
      ) %>% 
      slice(-1) %>%
      ungroup() %>%
      mutate(
        line = st_sfc(purrr::map2(
          .x = geometry, 
          .y = geometry_lagged, 
          .f = ~{st_union(c(.x, .y)) %>% st_cast("LINESTRING")}
        ))) %>% 
      select(id, line, competency, dispersaltime)
    
    tmp3 <- tmp2 %>%
      st_sf(geometry = st_sfc(tmp2$line, crs = st_crs(tmp2))) |> 
      arrange(id, dispersaltime)
    
  if(multilinestring==TRUE){
    
    tmp4 <- tmp3 |> group_by(id, competency) |> group_by(id, competency) |> summarise(do_union=FALSE)
    return(tmp4)
  } else {
    return(tmp3)
  }
    
    options(dplyr.summarise.inform = TRUE)
}



  