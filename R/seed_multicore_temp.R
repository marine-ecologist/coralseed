#' Seed futures
#'
#' Combined function to seed particles without returning extra data
#'
#' seed_particles() uses predict_competency(), simulate_mortality()
#'
#'
#' @param input input
#' @param limit.time limit the time series, for example 720 will limit the settlement results between 0-12hrs (defaults to "NA")
#' @param set.seed set seed for consistent results (defaults to NULL)
#' @param ... passes functions
#' @export
#'
#'


seed_multicore_temp <- function(
    input=NULL, set.seed=NULL, limit.time = NA, ...) {
  ### #1 extract_particle_points
  # set up particles for single point / time releases
  # Connie does not allow to disperse larvae at initial time of release < than 1 hour
  # so add a zero point at centroid of particle release area and set to t0 for
  # particles that aren't currently t0.

  #load(".../R/sysdata.rda")

  set.seed=set.seed

  if (is.null(set.seed) == TRUE) {
    set.seed(sample(-9999999:9999999, 1))
  }

  load_particles <- input

  # get details from input
  t0 <- min(load_particles$time)
  tmax <- max(load_particles$time)
  n_id <- length(unique(load_particles$id))

  # set dispersaltime
  load_particles <- load_particles |>
    dplyr::mutate(dispersaltime = as.numeric(time - min(t0)) / 60)

  if (!is.na(limit.time)) {
    load_particles <- load_particles |>
      dplyr::filter(dispersaltime <= limit.time * 60)
  }

  load_particles_t0 <- load_particles |>
    dplyr::filter(time == min(t0)) |>
    dplyr::summarize(geometry = sf::st_union(geometry)) |>
    sf::st_centroid() |>
    tidyr::uncount(n_id) |>
    dplyr::mutate(
      id = dplyr::row_number() - 1,
      time = min(t0),
      dispersaltime = 0
    ) |>
    sf::st_as_sf() |>
    sf::st_transform(20353)

  # bind original particles with new t0
  particle_points <- load_particles |>
    dplyr::filter(time > min(t0)) |>
    rbind(load_particles_t0) |>
    dplyr::arrange(id, dispersaltime)



  coords <- sf::st_coordinates(particle_points) |>
    as.data.frame()
  particle_points <- bind_cols(particle_points, coords)

  print(head(particle_points))

  }
