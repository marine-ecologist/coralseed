#' Test function
#' @param input input
#' @export
#'

test_functioning <- function(input){
  
  if (input %in% "nc") {
    input <- sf::st_read(system.file("shape/nc.shp", package="sf")) 
  } else if (input %in% "mermaid") {
    input <- coralseed:::Mermaid_PointSource_Bay_01 
  } else {
    input <- sf::st_read(input, drivers = "GeoJSON", quiet = TRUE)
  }
  
  tmp <- input |> 
    st_union()
  
  plot(tmp)
  return(tmp)
  
}