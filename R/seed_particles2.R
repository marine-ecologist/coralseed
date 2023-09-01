#' Seed particles
#'
#' Function to seed particles
#'
#' seed_particles() uses predict_competency(), simulate_mortality()
#'
#'
#' @param input input
#' @param seascape shp file inputs from seascape_probability()
#' @param simulate.mortality set mortality type via simulate_mortality() one of "typeI","typeII", "typeIII" (defaults to "none")
#' @param simulate.mortality.n set proportion of corals to kill over a 24hr period, where 0 is none, 1 is 100 (defaults to 0.1 or 10%)
#' @param competency.function set distribution to define competency from time-to-settlement model, one of "weibull", "exp", "log" (defaults to "exp")
#' @param limit.time limit the time series, for example 720 will limit the settlement results between 0-12hrs (defaults to "NA")
#' @param set.centre  reset CONNIE input to have a central t0 point (defaults to TRUE)
#' @param silent silence printing results while running (defaults to FALSE)
#' @param set.seed set seed for consistent results (defaults to NULL)
#' @param return.plot return outputs for seed_particles (defaults to "FALSE")
#' @param ... passes functions
#' @export
#'



seed_particles2 <- function(
    input=NULL, ...) {
  ##########################################################################################
  ### #1 extract_particle_points
  # set up particles for single point / time releases
  # Connie does not allow to disperse larvae at initial time of release < than 1 hour
  # so add a zero point at centroid of particle release area and set to t0 for
  # particles that aren't currently t0.

  if (input %in% "mermaid") {
    load_particles <- coralseed:::Mermaid_PointSource_Bay_01 |> 
      sf::st_zm(drop = TRUE, what = "ZM") |>
      sf::st_transform(20353) |>
      #dplyr::select(-decay_value) |>
      dplyr::mutate(time = time + lubridate::hours(14))
    } else {
    load_particles <- sf::st_read(input, drivers = "GeoJSON", quiet = TRUE) |> 
      sf::st_zm(drop = TRUE, what = "ZM") |>
      sf::st_transform(20353) |>
      dplyr::select(-decay_value) |>
      dplyr::mutate(time = time + lubridate::hours(14))
    
  } 
plot(load_particles)
return(load_particles)

  }
