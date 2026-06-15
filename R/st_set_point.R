#' st_set_point
#'
#' Function to generate sf point from a lon, lat, and a crs
#'
#' @param lon longitude
#' @param lat latitude
#' @param crs coordinate reference system
#' @export
#'

st_set_point <- function(lon, lat, crs=20353){

  point <- sf::st_point(c(lon, lat))
  sf_point <- sf::st_sf(geometry = sf::st_sfc(point, crs = crs))
  return(sf_point)

}
