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

  point <- st_point(c(lon, lat))
  sf_point <- st_sf(geometry = st_sfc(point, crs = crs))
  return(sf_point)

}
