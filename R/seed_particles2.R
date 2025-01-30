#' Seed particles
#'
#' Function to seed particles
#'
#' seed_particles() uses predict_competency(), simulate_mortality()
#'
#'
#' @param input input
#' @param example example
#' @param subsample subsample to n samples
#' @param seascape shp file inputs from seascape_probability()
#' @param simulate.mortality set mortality type via simulate_mortality() one of "typeI","typeII", "typeIII" (defaults to "none")
#' @param simulate.mortality.n set proportion of corals to kill over a 24hr period, where 0 is none, 1 is 100 (defaults to 0.1 or 10%)
#' @param competency.function set distribution to define competency from time-to-settlement model, one of "weibull", "exp", "log" (defaults to "exp")
#' @param limit.time limit the time series, for example 720 will limit the settlement results between 0-12hrs (defaults to "NA")
#' @param set.centre  reset CONNIE input to have a central t0 point (defaults to TRUE)
#' @param silent silence printing results while running (defaults to FALSE)
#' @param seed.value set seed for consistent results (defaults to NULL)
#' @param return.plot return outputs for seed_particles (defaults to "FALSE")
#' @param ... passes functions
#' @export
#'



seed_particles2 <- function(
    input = NULL, format = NULL, seascape = NULL, subsample = NULL,
    simulate.mortality = "none", simulate.mortality.n = 0.1,
    competency.function = "exponential", b_Intercept=NULL, limit.time = NA,
    set.centre = TRUE, seed.value = NULL,
    silent = FALSE, return.plot = FALSE, ...){
  ##########################################################################################
  ### 1. extract_particle_points
  # set up particles for single point / time releases
  # Connie does not allow to disperse larvae at initial time of release < than 1 hour
  # so add a zero point at centroid of particle release area and set to t0 for
  # particles that aren't currently t0.

  #load(".../R/sysdata.rda")

  # get(".Random.seed", envir = globalenv())
  # rm(.Random.seed, envir = globalenv())




  ### set seed

  set.seed(seed.value)
  #print(paste0("! is.null seed = ", get(".Random.seed", envir = globalenv())[10]))

  ### set input
  if (is.null(input) == FALSE) {

    if ("decay_value" %in% names(input)) {
      load_particles <- input |>
        sf::st_zm(drop = TRUE, what = "ZM") |>
        sf::st_transform(20353) |>
        dplyr::select(-decay_value)
    } else {
      load_particles <- input |>
        sf::st_zm(drop = TRUE, what = "ZM") |>
        sf::st_transform(20353)
    }


  }

  ### set example
  data_sources <- list(
    mermaid = Mermaid_PointSource_Bay_01,
    watson = WatsonN_PointSource_ForeReefSh_01,
    palfrey = PalfreyN_PointSource_ForeReefEx_01,
    spawnhub = SpHub_PointSource_SELaggon_01,
    clamgarden = ClamGarden_PointSource_OpenLagoon_01
  )

  data_sources_df <- data.frame(
    dataset_name = c("mermaid", "watson", "palfrey", "spawnhub", "clamgarden"),
    linked_file_name = c(
      "Mermaid_PointSource_Bay_01",
      "WatsonN_PointSource_ForeReefSh_01",
      "PalfreyN_PointSource_ForeReefEx_01",
      "SpHub_PointSource_SELaggon_01",
      "ClamGarden_PointSource_OpenLagoon_01"
    ),
    stringsAsFactors = FALSE
  )

  if (is.null(example) == TRUE) {
    load_particles <- load_particles
  } else if (example %in% names(data_sources)) {
    load_particles <- data_sources[[example]] %>%
      sf::st_zm(drop = TRUE, what = "ZM") %>%
      sf::st_transform(20353)
  }

  ### initiate
  # get details from input
  t0 <- min(load_particles$time)
  tmax <- max(load_particles$time)
  n_id <- length(unique(load_particles$id))

  # set dispersaltime
  load_particles <- load_particles |>
    dplyr::mutate(dispersaltime = as.numeric(time - min(t0)) / 60)


    load_particles_t0 <- load_particles |>
      dplyr::filter(time == min(t0)) |>
      dplyr::summarize(geometry = sf::st_union(geometry)) |>
      sf::st_centroid() |>
      tidyr::uncount(n_id) |>
      dplyr::mutate(
        id = unique(load_particles$id),
        time = min(t0),
        dispersaltime = 0
      ) |>
      sf::st_as_sf() |>
      sf::st_transform(20353)


    # bind original particles with new t0
    particle_points <- load_particles |>
      #dplyr::select(-decay_value) |>
      dplyr::filter(time > min(t0)) |>
      rbind(load_particles_t0) |>
      dplyr::arrange(id, dispersaltime)




  ##########################################################################################
  ### 2. Predict competency
  #print(seed.value)
  competency_times_output <- predict_competency(
    n_id = length(unique(particle_points$id)), n_sims = 1000, b_Intercept = b_Intercept,
    competency.function = competency.function, seed.value = seed.value, return.plot = return.plot
  )
  # check:
  # hist(competency_times_output$simulated_settlers$settlement_point)

  competency_times <- competency_times_output |>
    with(simulated_settlers) |>
    dplyr::sample_frac(size = 1) |>
    dplyr::mutate(id = unique(as.factor(particle_points$id))) |>
    dplyr::mutate(id = as.factor(id))


 # interpolate between particle points by 1 minute intervals and bind probability outputs
  particle_points_expanded <- particle_points |>
    as.data.frame() |>
    dplyr::mutate(geometry = gsub("[^0-9. -]", "", geometry), id = as.factor(id)) |>
    tidyr::separate(geometry, into = c("X", "Y"), sep = " ", convert = TRUE) |>
    dplyr::group_by(id) |>
    tidyr::complete(time = seq(min(time), max(time), by = "1 mins")) |>
    dplyr::mutate(X = approx(time, X, time)$y, Y = approx(time, Y, time)$y) |>
    dplyr::mutate(dispersaltime = as.integer(time - min(particle_points$time)) / 60) |>
    dplyr::ungroup() |>
    dplyr::left_join(competency_times, by = "id") |> # join with probability
    dplyr::mutate(state = ifelse(dispersaltime <= settlement_point, 0, 1)) |>
    dplyr::mutate(competency = (dplyr::recode(as.numeric(state), "1" = "competent", "0" = "incompetent"))) |> ####### intermittent error here, check for NA on loop
    dplyr::arrange(id) |>
    sf::st_as_sf(coords = c("X", "Y"), crs = 20353) |>
    sf::st_cast("POINT")


  ##########################################################################################
  ### 2. Predict mortality

  #print(seed.value)

  particle_points_expanded_postmortality <- simulate_mortality(
    input = particle_points_expanded,
    simulate.mortality = simulate.mortality, simulate.mortality.n = simulate.mortality.n,
    return.plot = return.plot, seed.value = seed.value, silent = TRUE)

  # check output when simulate.mortality.n > 0
  # hist(particle_points_expanded_postmortality$simulated_mortality$settlement_point)

  ##########################################################################################
  ### 3. Settle particles by probability

  # join with spatial probability maps
  particle_points_probability <- sf::st_join((particle_points_expanded_postmortality |> with(simulated_mortality)), seascape) |>
    dplyr::mutate(class = forcats::fct_na_value_to_level(class, "Ocean")) |> # replace NA with Ocean
    dplyr::mutate(habitat_id = forcats::fct_na_value_to_level(habitat_id, "Ocean")) |> # replace NA with Ocean
    dplyr::mutate(settlement_probability = tidyr::replace_na(settlement_probability, 0)) |> # make lagoon and ocean 0
    dplyr::mutate(state = as.numeric(as.character(state))) |>
    dplyr::mutate(settlement_outcome = as.numeric(as.character(as.factor(rbinom(length(state), size = 1, prob = settlement_probability))))) |>
    dplyr::mutate(final = (state * settlement_outcome)) |> # turn into probability
    dplyr::mutate(outcome = as.factor(state * settlement_outcome))

  # add triple-letter string to ID for unique df (p=0.00006 random draw)
  idstring <- paste0(sample(LETTERS, 1, replace = TRUE), sample(LETTERS, 1, replace = TRUE), sample(LETTERS, 1, replace = TRUE))
  particle_points_probability$id <- paste0(idstring, particle_points_probability$id)


  set.seed(NULL)
  return(particle_points_probability)
}
