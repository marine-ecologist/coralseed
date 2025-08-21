#' Seed particles
#'
#' Function to seed particles
#'
#' seed_particles() uses predict_competency(), simulate_mortality()
#'
#'
#' seed_particles_dt(input = "Users/rof011/coralseed/data-raw/run_day_11656_lizard_fcst_15_2611_26.json",
#'                seascape = seascape,
#'                brmsfit=infamis_tiles_exp,
#'                simulate.mortality = "typeI",
#'                simulate.mortality.n = 0.1)
#'
#' @param input input
#' @param seascape shp file inputs from seascape_probability()
#' @param subsample subsample to n samples
#' @param simulate.mortality set mortality type via simulate_mortality() one of "typeI","typeII", "typeIII" (defaults to "none")
#' @param simulate.mortality.n set proportion of corals to kill over a 24hr period, where 0 is none, 1 is 100 (defaults to 0.1 or 10%)
#' @param brmsfit a brmsfit model for predicting competency
#' @param set_b_Intercept manually set intercept (for development)
#' @param limit.time limit the time series (e.g. 720 mins)
#' @param set.centre reset CONNIE input to have a central t0 point (defaults to TRUE)
#' @param seed.value set seed for consistent results (defaults to NULL)
#' @param crs coordinate reference system, default EPSG:32755
#' @param interval time interval for interpolating paths
#' @param silent silence printing results (defaults to TRUE)
#' @param return.summary return summary table
#' @param return.plot return plot object
#' @param save.plot path to save plot image
#' @param plot.width width of saved plot
#' @param plot.height height of saved plot
#' @param ... passes functions
#' @export

seed_particles_dt <- function(
    input = NULL, seascape = NULL, subsample = NULL,
    simulate.mortality = "none", simulate.mortality.n = 0.1,
    brmsfit=infamis_tiles_exp, set_b_Intercept=NULL, limit.time = NA,
    set.centre = TRUE, seed.value = NULL, crs=32755, interval="1 mins",
    silent = TRUE, return.summary = FALSE, return.plot = FALSE,
    save.plot=FALSE, plot.width=12, plot.height=7.5, ...){

  set.seed(seed.value)

  load_particles <- tryCatch({
    sf_data <- input
    if (any(sf::st_z_range(sf_data) != 0)) {
      sf_data <- sf::st_zm(sf_data, drop = TRUE, what = "ZM")
    }
    sf::st_transform(sf_data, crs)
  }, error = function(e) {
    message("Error loading spatial data: ", conditionMessage(e))
    cat("\n\n error: coralseed requires a valid input file in sf format (either .json (e.g. CONNIE output) or .zarr (e.g. oceanparcels output))) \n\n\n\n")
    return(NULL)
  })

  if ("decay_value" %in% names(load_particles)) {
    load_particles <- load_particles |> dplyr::select(-decay_value)
  }

  if (is.null(load_particles)) {
    cat("\n\n error: coralseed requires a valid input file either in .json (e.g. CONNIE output) or .zarr (e.g. oceanparcels output)) \n\n\n\n")
    stop()
  }

  if (!is.null(subsample)) {
    load_particles <- load_particles |>
      dplyr::mutate(id = as.factor(id)) |>
      dplyr::filter(id %in% sample(x = as.numeric(unique(load_particles$id)), size = as.numeric(subsample))) |>
      dplyr::mutate(id = as.integer(id))
  }

  tic()
  t0 <- min(load_particles$time)
  tmax <- max(load_particles$time)
  n_id <- length(unique(load_particles$id))

  load_particles <- load_particles |> dplyr::mutate(dispersaltime = as.numeric(time - min(t0)) / 60)

  if (!is.na(limit.time)) {
    load_particles <- load_particles |> dplyr::filter(dispersaltime <= limit.time * 60)
  }

  if (set.centre == TRUE) {
    load_particles_t0 <- load_particles |>
      dplyr::filter(time == min(t0)) |>
      dplyr::summarize(geometry = sf::st_union(geometry)) |>
      sf::st_centroid() |>
      tidyr::uncount(n_id) |>
      dplyr::mutate(id = unique(load_particles$id), time = min(t0), dispersaltime = 0) |>
      sf::st_as_sf() |>
      sf::st_transform(crs)

    particle_points <- load_particles |>
      dplyr::filter(time > min(t0)) |>
      rbind(load_particles_t0) |>
      dplyr::arrange(id, dispersaltime)
  } else {
    particle_points <- load_particles |> dplyr::arrange(id, dispersaltime)
  }

  max.time <- ifelse(is.na(limit.time), max(particle_points$dispersaltime), limit.time * 60)

  competency_times_output <- predict_competency(
    input = brmsfit, n_particles = length(unique(particle_points$id)),
    max.time = max.time, seed.value = seed.value, return.plot = return.plot
  )

  competency_times <- competency_times_output |> with(simulated_settlers) |> dplyr::sample_frac(1) |> dplyr::mutate(id = as.factor(unique(particle_points$id)))

  toc()
  particle_points_expanded <- particle_points |>
    as.data.frame() |>
    dplyr::mutate(geometry = gsub("[^0-9. -]", "", geometry), id = as.factor(id)) |>
    tidyr::separate(geometry, into = c("X", "Y"), sep = " ", convert = TRUE) |>
    dplyr::group_by(id) |>
    tidyr::complete(time = seq(min(time), max(time), by = interval)) |>
    dplyr::mutate(X = approx(time, X, time)$y, Y = approx(time, Y, time)$y) |>
    dplyr::mutate(dispersaltime = as.integer(time - min(particle_points$time)) / 60) |>
    dplyr::ungroup() |>
    dplyr::left_join(competency_times, by = "id") |>
    dplyr::mutate(state = ifelse(dispersaltime <= settlement_point, 0, 1)) |>
    dplyr::mutate(competency = dplyr::recode(as.numeric(state), "1" = "competent", "0" = "incompetent")) |>
    dplyr::arrange(id) |>
    sf::st_as_sf(coords = c("X", "Y"), crs = crs) |>
    sf::st_cast("POINT")


  mortality_count <- particle_points_expanded_postmortality$survivorship_output |>
    dplyr::filter(mortalitytime < max.time) |> nrow()

  particle_points_probability <- sf::st_join(
    particle_points_expanded_postmortality |> with(simulated_mortality),
    seascape |> sf::st_transform(crs)
  ) |>
    dplyr::mutate(
      class = forcats::fct_na_value_to_level(class, "Ocean"),
      habitat_id = forcats::fct_na_value_to_level(habitat_id, "Ocean"),
      settlement_probability = tidyr::replace_na(settlement_probability, 0),
      state = as.numeric(as.character(state)),
      settlement_outcome = rbinom(length(state), 1, settlement_probability),
      final = state * settlement_outcome,
      outcome = as.factor(state * settlement_outcome)
    ) |>
    dplyr::mutate(id = paste0(paste0(sample(LETTERS, 3, replace = TRUE), collapse=""), id))

  set.seed(NULL)

  return_list <- list(seed_particles = particle_points_probability)
  if (return.plot) return_list$multiplot <- competency_times_output$simulated_settlers_plot
  if (return.summary) {
    summary_table <- data.frame(
      Metric = c("Number of particle tracks", "Seed setting", "Start time", "End time", "Dispersal time (hrs)", "Total mortality by tmax"),
      Value = c(
        length(unique(load_particles$id)),
        if (is.null(seed.value)) "[No seed set]" else paste0("[seed = ", seed.value, "]"),
        as.character(t0),
        as.character(tmax),
        as.numeric(tmax - t0),
        as.numeric(mortality_count)
      )
    )
    return_list$summary <- summary_table
  }

  if (length(return_list) == 1) return(return_list[[1]]) else return(return_list)
}

