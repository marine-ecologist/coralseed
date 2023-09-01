#' Calculate dispersion
#'
#' Function to calculate dispersion of settled points
#'
#' dispersion = standard deviation of distances from each point to centroid of all points
#'
#' @param input sf POINTS file (defaults to NULL)
#' @export
#'
calculate_dispersion <- function(input){


x_coords <- sapply(input$geometry, function(point) sf::st_coordinates(point)[1])
y_coords <- sapply(input$geometry, function(point) sf::st_coordinates(point)[2])

centroid_x <- mean(x_coords)
centroid_y <- mean(y_coords)

squared_distances <- (x_coords - centroid_x)^2 + (y_coords - centroid_y)^2
variance <- sum(squared_distances) / length(squared_distances)
standard_deviation <- sqrt(variance)

return(standard_deviation)
}


