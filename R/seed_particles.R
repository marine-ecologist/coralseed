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



seed_particles <- function(
    input = NULL, example = NULL, seascape = NULL, subsample = NULL,
    simulate.mortality = "none", simulate.mortality.n = 0.1,
    competency.function = "exp", limit.time = NA,
    set.centre = TRUE, set.seed = NULL,
    silent = FALSE, return.plot = FALSE, ...) {
  ##########################################################################################
  ### #1 extract_particle_points
  # set up particles for single point / time releases
  # Connie does not allow to disperse larvae at initial time of release < than 1 hour
  # so add a zero point at centroid of particle release area and set to t0 for
  # particles that aren't currently t0.

  if (is.null(set.seed) == TRUE) {
    set.seed(sample(-9999999:9999999, 1))
  }


  if (is.null(input) == FALSE) {
    load_particles <- input |>
      sf::st_zm(drop = TRUE, what = "ZM") |>
      sf::st_transform(20353) |>
      dplyr::select(-decay_value)
  } else {
  }

  data_sources <- list(
    mermaid = coralseed:::Mermaid_PointSource_Bay_01,
    watson = coralseed:::WatsonN_PointSource_ForeReefSh_01,
    palfrey = coralseed:::PalfreyN_PointSource_ForeReefEx_01,
    spawnhub = coralseed:::SpHub_PointSource_SELaggon_01,
    clamgarden = coralseed:::ClamGarden_PointSource_OpenLagoon_01
  )

  data_sources_df <- data.frame(
    dataset_name = c("mermaid", "watson", "palfrey", "spawnhub", "clamgarden"),
    linked_file_name = c(
      "coralseed:::Mermaid_PointSource_Bay_01",
      "coralseed:::WatsonN_PointSource_ForeReefSh_01",
      "coralseed:::PalfreyN_PointSource_ForeReefEx_01",
      "coralseed:::SpHub_PointSource_SELaggon_01",
      "coralseed:::ClamGarden_PointSource_OpenLagoon_01"
    ),
    stringsAsFactors = FALSE
  )


  if (is.null(example) == TRUE) {

  } else if (example %in% names(data_sources)) {
    load_particles <- data_sources[[example]] %>%
      sf::st_zm(drop = TRUE, what = "ZM") %>%
      sf::st_transform(20353) %>%
      dplyr::mutate(time = time + lubridate::hours(14))
    # if (is.null(silent) == FALSE) {
    #   cat(paste0("Example: ", data_sources_df[data_sources_df$dataset_name == example, 2], " \n"))
    # }
  } else if (!example %in% names(data_sources)) {
    cat("\n error: example not found, must be one of mermaid, watson, palfrey, spawnhub, clamgarden. \n\n")
  }

  if (is.null(load_particles) == TRUE) {
    cat("\n\n error: coralseed requires either an input file or an example file\n\n\n\n")
    stop()
  } else {

  }

  if (is.null(subsample) == FALSE) {
    load_particles <- load_particles |>
      dplyr::mutate(id = as.factor(id)) |>
      dplyr::filter(id %in% sample(x = as.numeric(unique(load_particles$id)), size = as.numeric(subsample))) |>
      dplyr::mutate(id = as.integer(id))
  } else { }

  # get details from input
  t0 <- min(load_particles$time)
  tmax <- max(load_particles$time)
  n_id <- length(unique(load_particles$id))

  # print details while loading
  if (silent == FALSE) {
    (cat(paste0("\n")))
    # (cat(paste0("Filename: ", basename(input), " \n")))
    (cat(paste0("Importing ", length(levels(as.factor(load_particles$id))), " particle tracks", "\n")))
    if (is.null(set.seed) == TRUE) {
      (cat(paste0("[No seed set, random draws used] \n")))
    } else {
      (cat(paste0("[seed = ", set.seed, "] \n")))
    }
    (cat(paste0("\n")))
    (cat(paste0("Time start = ", t0, "\n")))
    (cat(paste0("Time end = ", tmax, "\n")))
    (cat(paste0("Total dispersaltime (hrs) = ", tmax - t0, "\n")))

    if (!is.na(limit.time)) {
      (cat(paste0("Particle tracks limited to ", limit.time, " hrs \n")))
    }
  }
  # set dispersaltime
  load_particles <- load_particles |>
    dplyr::mutate(dispersaltime = as.numeric(time - min(t0)) / 60)

  if (!is.na(limit.time)) {
    load_particles <- load_particles |>
      dplyr::filter(dispersaltime <= limit.time * 60)
  }


  if (set.centre == TRUE) {
    # extract centroid for release point
    # load_particles_t0 <- load_particles |>
    #   dplyr::filter(time == min(t0)) |>
    #   dplyr::summarize(geometry = sf::st_union(geometry)) |>
    #   sf::st_centroid() |>
    #   tidyr::uncount(n_id) |>
    #   dplyr::mutate(
    #     id = dplyr::row_number() - 1,
    #     time = min(t0),
    #     dispersaltime = 0
    #   ) |>
    #   sf::st_as_sf() |>
    #   sf::st_transform(20353)


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
      dplyr::filter(time > min(t0)) |>
      rbind(load_particles_t0) |>
      dplyr::arrange(id, dispersaltime)
  } else if (set.centre == FALSE) {
    particle_points <- load_particles |>
      dplyr::arrange(id, dispersaltime)
  }
  ##########################################################################################
  ###  #2 Predict competency

  competency_times_output <- predict_competency(
    n_id = length(levels(as.factor(particle_points$id))),
    competency.function = competency.function, set.seed = set.seed, return.plot = return.plot
  )

  # head(competency_times_output$simulated_settlers)

  competency_times <- competency_times_output |>
    with(simulated_settlers) |>
    dplyr::sample_frac(size = 1) |>
    dplyr::mutate(id = (unique(as.factor(particle_points$id))))

  t6 <- (sum(competency_times$settlement_point < 360)) # / n_id) #* 100
  t12 <- (sum(competency_times$settlement_point < 720)) # / n_id) #* 100
  t24 <- (sum(competency_times$settlement_point < 1440)) # / n_id) #* 100

  if (silent == FALSE) {
    (cat(paste0("  \n")))
    (cat(paste0("Competency at t6 = ", t6, " / ", n_id, " larvae \n")))
    (cat(paste0("Competency at t12 = ", t12, " / ", n_id, " larvae \n")))
    (cat(paste0("Competency at t24 = ", t24, " / ", n_id, " larvae \n")))
    (cat(paste0("  \n")))
  }
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
    dplyr::mutate(competency = (dplyr::recode(state, "1" = "competent", "0" = "incompetent"))) |> ####### intermittent error here, check for NA on loop
    dplyr::arrange(id) |>
    sf::st_as_sf(coords = c("X", "Y"), crs = 20353) |>
    sf::st_cast("POINT")

  # tail(particle_points_expanded)

  ### add mortality

  # particle_points_expanded_postmortality <- particle_points_expanded
  particle_points_expanded_postmortality <- simulate_mortality(
    input = particle_points_expanded,
    simulate.mortality = simulate.mortality, simulate.mortality.n = simulate.mortality.n,
    return.plot = return.plot, set.seed = set.seed, silent = silent
  )


  ##########################################################################################
  #####  #3 Settle particles by probability
  # bind with benthic substrates
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




  if (return.plot == TRUE) {
    suppressWarnings({
      particle_points_probability_settlers <- particle_points_probability |>
        dplyr::group_by(id) |>
        dplyr::filter(outcome == 1) |>
        dplyr::slice(1)

      particle_points_probability_plot <- ggplot2::ggplot() +
        ggplot2::theme_bw() +
        ggplot2::geom_sf(data = particle_points_probability_settlers, size = 1.5, shape = 21, fill = "aquamarine3", alpha = 0.6)

      kde_settle <- ggplot2::ggplot() +
        ggplot2::theme_bw() +
        ggplot2::geom_histogram(data = particle_points_probability_settlers, ggplot2::aes(x = dispersaltime, y = ggplot2::after_stat(density)), color = "black", fill = "lightblue", binwidth = 5) +
        ggplot2::geom_density(data = particle_points_probability_settlers, ggplot2::aes(x = dispersaltime, y = ggplot2::after_stat(density)), color = "darkred", linewidth = 1.2) +
        ggplot2::labs( # title = "Kernel Density Estimation (KDE) of max dispersaltime for settled larvae",
          x = "Dispersal time (mins)",
          y = "Density"
        )


      p1 <- na.omit(competency_times_output$simulated_settlers_plot)
      p2 <- particle_points_expanded_postmortality$simulated_mortality_plot + ggplot2::ggtitle("2. Survival curve")
      p3 <- kde_settle + ggplot2::ggtitle("3. Dispersaltime prior to settlement")
      p4 <- particle_points_probability_plot + ggplot2::ggtitle("4. Spatial pattern settlers")

      print(cowplot::plot_grid(p1, p2, p3, p4, align = "lr", ncol = 2, nrow = 2))

      # print(ggpubr::ggarrange(
      #   na.omit(competency_times_output$simulated_settlers_plot),
      #   particle_points_expanded_postmortality$simulated_mortality_plot + ggplot2::ggtitle("2. Survival curve"),
      #   kde_settle + ggplot2::ggtitle("3. Dispersaltime prior to settlement"),
      #   particle_points_probability_plot + ggplot2::ggtitle("4. Spatial pattern settlers"),
      #   ncol = 2, nrow = 2
      # ))
    })
  }

  #
  # data.frame(descriptors=c("Filename", "n particle tracks", "", "Time start", "Time end", "Total dispersaltime (hrs)", "Time limit", "", "Competency at t6", "Competency at t12", "Competency at t24"),
  #          values=c(basename(input), length(levels(as.factor(load_particles$id))), "", set.seed, t0, tmax, tmax-t0, limit.time, "", t6, t12, t24) ) |>
  #   mmtable(values) |> mmtable::header_left(everything())
  #
  #

  return(particle_points_probability)
}
