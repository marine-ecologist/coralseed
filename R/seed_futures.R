#' Seed futures
#'
#' Combined function to seed particles without returning extra data
#'
#' seed_particles() uses predict_competency(), simulate_mortality()
#'
#'
#' @param input input
#' @param example example input
#' @param seascape shp file inputs from seascape_probability()
#' @param simulate.mortality set mortality type via simulate_mortality() one of "typeI","typeII", "typeIII" (defaults to "none")
#' @param simulate.mortality.n set proportion of corals to kill over a 24hr period, where 0 is none, 1 is 100 (defaults to 0.1 or 10%)
#' @param competency.function set distribution to define competency from time-to-settlement model, one of "weibull", "exp", "log" (defaults to "exp")
#' @param limit.time limit the time series, for example 720 will limit the settlement results between 0-12hrs (defaults to "NA")
#' @param probability one of "additive", "rapid", "lagged")
#' @param tracks  generate particle track outputs (multilinestrings)
#' @param set.seed set seed for consistent results (defaults to NULL)
#' @param ... passes functions
#' @export
#'
#'

seed_futures <- function(
    input = NULL, example = NULL, seascape = NULL, simulate.mortality = "none", simulate.mortality.n = 0.1,
    competency.function = "exp", limit.time = NA, probability = "additive", tracks = FALSE,
    set.seed = NULL, ...) {
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
    load_particles <- (input)
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
      "Mermaid_PointSource_Bay_01",
      "WatsonN_PointSource_ForeReefSh_01",
      "PalfreyN_PointSource_ForeReefEx_01",
      "SpHub_PointSource_SELaggon_01",
      "ClamGarden_PointSource_OpenLagoon_01"
    ),
    stringsAsFactors = FALSE
  )


  if (is.null(example) == TRUE) {

  } else if (example %in% names(data_sources)) {
    load_particles <- data_sources[[example]] %>%
      sf::st_zm(drop = TRUE, what = "ZM") %>%
      sf::st_transform(20353) %>%
      dplyr::mutate(time = time + lubridate::hours(14))
  } else if (!example %in% names(data_sources)) {
    cat("\n error: example not found, must be one of mermaid, watson, palfrey, spawnhub, clamgarden. \n\n")
  }

  if (is.null(load_particles) == TRUE) {
    cat("\n\n error: coralseed requires either an input file or an example file\n\n\n\n")
    stop()
  } else {

  }
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


  ##########################################################################################
  ###  #2 Predict competency

  n_id <- length(levels(as.factor(particle_points$id)))

  if (competency.function == "exponential") {
    posterior_draws <- {
      coralseed::parameter_draws_exp
    }
    draw_individuals <- function(row) {
      rexp(1, rate = 1 / exp(row["b_Intercept"]))
    }
    random_draws <- posterior_draws |>
      dplyr::slice_sample(n = n_id)
    all_samples <- apply(random_draws, 1, draw_individuals)
    simulated_settlers <- data.frame(settlement_point = ceiling(all_samples)) |>
      dplyr::mutate(id = as.factor(rev(seq(0, n_id - 1, 1))))
  } else if (competency.function == "logarithmic") {
    set.seed(set.seed)
    posterior_draws <- coralseed::parameter_draws_log
    draw_individuals <- function(row) {
      rlnorm(1, row["b_Intercept"])
    }
    random_draws <- posterior_draws |> dplyr::slice_sample(n = n_id)
    all_samples <- apply(random_draws, 1, draw_individuals)
    simulated_settlers <- data.frame(settlement_point = ceiling(all_samples)) |>
      dplyr::mutate(id = as.factor(rev(seq(0, n_id - 1, 1))))
  } else if (competency.function == "weibull") {
    set.seed(set.seed)
    posterior_draws <- coralseed::parameter_draws_weibull
    draw_individuals <- function(row) {
      rweibull(1, shape = row["shape"], scale = row["scale"])
    }
    random_draws <- posterior_draws |> dplyr::slice_sample(n = n_id)
    all_samples <- apply(random_draws, 1, draw_individuals)

    simulated_settlers <- data.frame(settlement_point = ceiling(all_samples)) |>
      dplyr::mutate(id = as.factor(rev(seq(0, n_id - 1, 1))))
  }

  competency_times <- simulated_settlers |>
    with(simulated_settlers) |>
    dplyr::mutate(id = (unique(as.factor(particle_points$id))))

  data.table::setDT(competency_times) # move elsewhere?


  # # interpolate between particle points by 1 minute intervals and bind probability outputs
  # particle_points_expanded <- particle_points |>
  #   as.data.frame() |>
  #   dplyr::mutate(geometry = gsub("[^0-9. -]", "", geometry), id = as.factor(id)) |>
  #   tidyr::separate(geometry, into = c("X", "Y"), sep = " ", convert = TRUE) |>
  #   dplyr::group_by(id) |>
  #   tidyr::complete(time = seq(min(time), max(time), by = "1 mins")) |>
  #   dplyr::mutate(X = approx(time, X, time)$y, Y = approx(time, Y, time)$y) |>
  #   dplyr::mutate(dispersaltime = as.integer(time - min(particle_points$time)) / 60) |>
  #   dplyr::ungroup() |>
  #   dplyr::left_join(competency_times, by = "id") |> # join with probability
  #   dplyr::mutate(state = ifelse(dispersaltime <= settlement_point, 0, 1)) |>
  #   dplyr::mutate(competency = (dplyr::recode(state, "1" = "competent", "0" = "incompetent"))) |>
  #   dplyr::arrange(id) |>
  #   sf::st_as_sf(coords = c("X", "Y"), crs = 20353) |>
  #   sf::st_cast("POINT")

  coords <- do.call(rbind, sf::st_geometry(particle_points)) |>
    data.table::as.data.table() |>
    setNames(c("X", "Y"))

  # Start by extracting the dataframe from the sp object
  particle_points_df <- as.data.frame(particle_points) |> mutate(id = as.factor(id))
  particle_points_dt <- data.table::setDT(particle_points_df)
  particle_points_dt <- cbind(particle_points_dt, coords)

  # particle_points_dt[, geometry := NULL]

  all_times <- particle_points_dt[, .(time = seq(min(particle_points_dt$time), max(particle_points_dt$time), by = "1 min")), by = id]
  particle_points_dt <- particle_points_dt[all_times, on = c("id", "time")]
  particle_points_dt[, dispersaltime := as.integer(time - min(particle_points_dt$time)) / 60]

  particle_points_dt <- particle_points_dt[, `:=`(
    X = approx(time, X, time)$y,
    Y = approx(time, Y, time)$y
  ), by = id]


  particle_points_dt[competency_times, settlement_point := settlement_point, on = "id"]
  particle_points_dt[, state := ifelse(dispersaltime <= settlement_point, 0, 1), by = id]
  particle_points_dt[, competency := ifelse(state == 1, "competent", "incompetent")]
  particle_points_dt <- particle_points_dt[!is.na(X)]

  particle_points_expanded <- sf::st_as_sf(particle_points_dt, coords = c("X", "Y"))



  n_mortality <- length(levels(unique(as.factor(particle_points_expanded$id)))) * simulate.mortality.n
  dead_id_levels <- sample(levels(particle_points_expanded$id), n_mortality)
  n_id <- length(unique(particle_points_expanded$id))

  df_dead <- particle_points_expanded |>
    dplyr::filter(id %in% dead_id_levels) |>
    tidyr::drop_na(id) # subset dead particles
  df_alive <- particle_points_expanded |>
    dplyr::filter(!(id %in% dead_id_levels)) |>
    tidyr::drop_na(id) # keep live particles

  survivorship_type <- function(n, shape, scale) {
    dispersaltime <- seq(1, 1440, 1)
    probabilities <- dweibull(dispersaltime, shape, scale)
    sample(dispersaltime, size = n, prob = probabilities, replace = TRUE)
  }

  # Fit the types to the individuals and sample the time of death
  set.seed(set.seed)
  typeI_time <- survivorship_type(n_mortality, 2.5, 1440)
  typeII_time <- survivorship_type(n_mortality, 1.5, 1440)
  typeIII_time <- survivorship_type(n_mortality, 0.5, 1440)

  # Combine into a data frame
  survivorship_output <- data.frame(
    type = rep(c("typeI", "typeII", "typeIII"), each = n_mortality),
    mortalitytime = c(typeI_time, typeII_time, typeIII_time),
    id = sample(dead_id_levels)
  ) |> dplyr::filter(type == simulate.mortality)

  df_dead_timed <- df_dead |>
    dplyr::left_join(survivorship_output, by = "id") |>
    dplyr::filter(dispersaltime <= mortalitytime) |>
    dplyr::select(-mortalitytime, -type) |>
    dplyr::mutate(id = as.factor(id))

  particle_points_expanded_postmortality <- rbind(df_alive, df_dead_timed) |>
    sf::st_set_crs("EPSG:20353")


  ##########################################################################################
  #####  #3 Settle particles by probability
  # bind with benthic substrates
  particle_points_probability <- sf::st_join(particle_points_expanded_postmortality, seascape |> dplyr::mutate(class = as.factor(class))) |>
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


  if (probability == "additive") {
    select_particles <- particle_points_probability |>
      dplyr::filter(outcome == "1") |> # dplyr::select settled particles
      dplyr::arrange(id, time) |>
      dplyr::group_by(id, habitat_id) |>
      dplyr::slice_sample(n = 1) |> # randomly sample a point in each habitat where competent
      dplyr::arrange(id, dispersaltime) |>
      dplyr::group_by(id) |>
      dplyr::slice_head(n = 1) |> # take first of the habitats by dispersaltime if multiple intersects
      dplyr::select(id, class, time, dispersaltime) |>
      dplyr::mutate(cat = "settled", cat = as.factor(cat))
  } else if (probability == "rapid") {
    select_particles <- particle_points_probability |>
      dplyr::filter(outcome == "1") |> # dplyr::select settled particles
      dplyr::arrange(id, time) |>
      dplyr::group_by(id, habitat_id) |>
      dplyr::slice_min(n = 1, order_by = dispersaltime) |> # take first value
      dplyr::arrange(id, dispersaltime) |>
      dplyr::group_by(id) |>
      dplyr::slice_head(n = 1) |> # take first of the habitats by dispersaltime if multiple intersects
      dplyr::select(id, class, time, dispersaltime) |>
      dplyr::mutate(cat = "settled", cat = as.factor(cat))
  } else if (probability == "lagged") { # (10 minute random dplyr::selection)
    select_particles <- particle_points_probability |>
      dplyr::filter(outcome == "1") |> # dplyr::select settled particles
      dplyr::arrange(id, time) |>
      dplyr::group_by(id, habitat_id) |>
      dplyr::slice_min(n = 10, order_by = dispersaltime) |> # take first ten values
      dplyr::slice_sample(n = 1) |> # take first five values
      dplyr::arrange(id, dispersaltime) |>
      dplyr::group_by(id) |>
      dplyr::slice_head(n = 1) |> # take first of the habitats by dispersaltime if multiple intersects
      dplyr::select(id, class, time, dispersaltime) |>
      dplyr::mutate(cat = "settled", cat = as.factor(cat))
  } else {
    print("probability is one of: additive / rapid / lagged")
  }


  if (tracks == TRUE) {
    settled_tracks <- particle_points_probability |>
      dplyr::filter(id %in% unique(select_particles$id)) |>
      dplyr::left_join(select_particles |> as.data.frame() |> select(id, dispersaltime) |> rename(maxdispersaltime = dispersaltime), by = "id") |>
      dplyr::filter(dispersaltime < maxdispersaltime) |> # dplyr::select settled particles
      dplyr::arrange(id, time) |>
      dplyr::group_by(id) |>
      dplyr::filter(dplyr::n() >= 3) |> # from particles_to_tracks
      dplyr::group_by(id) |> # from particles_to_tracks
      dplyr::summarise(do_union = FALSE) |>
      sf::st_cast("MULTILINESTRING")

    results <- list(
      select_particles,
      settled_tracks
    )
    return(results)
  } else {
    return(select_particles)
  }
}
