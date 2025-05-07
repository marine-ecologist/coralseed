#' Seed futures
#'
#' Combined function to seed particles without returning extra data
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
#' @param probability one of "additive", "rapid", "lagged")
#' @param tracks  generate particle track outputs (multilinestrings)
#' @param set.seed set seed for consistent results (defaults to NULL)
#' @param ... passes functions
#' @export
#'
#'


seed_futures <- function(
    input=NULL, example=NULL, seascape = NULL, simulate.mortality = "none", simulate.mortality.n = 0.1,
    competency.function = "exp", limit.time = NA, probability = "additive", tracks = FALSE,
    set.seed = NULL, ...) {
  ############ 1. extract points ####
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


  if (is.null(input) == FALSE) {
    load_particles <- sf::st_read(input) |>
      sf::st_zm(drop = TRUE, what = "ZM") |>
      sf::st_transform(20353) |>
      dplyr::select(-decay_value)

  } else {
  }

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

  } else if (example %in% names(data_sources)) {

    load_particles <- as.data.table(data_sources[["mermaid"]])
    load_particles[, time := time + lubridate::hours(14)]

  } else if (!example %in% names(data_sources)) {
    cat("\n error: example not found, must be one of mermaid, watson, palfrey, spawnhub, clamgarden. \n\n")
  }

  if (is.null(load_particles) == TRUE) {
    cat("\n\n error: coralseed requires either an input file or an example file\n\n\n\n")
    stop()
  } else {

  }



  t0 <- min(load_particles$time)
  tmax <- max(load_particles$time)
  n_id <- length(unique(load_particles$id))

  load_particles <- as.data.table(load_particles)
  load_particles[, dispersaltime := as.numeric(time - min(t0)) / 60]

  if (!is.na(limit.time)) {
     load_particles <- load_particles[dispersaltime <= limit.time * 60]
   }

  load_particles[, c("X", "Y") := .(sf::st_coordinates(geometry)[, 1], sf::st_coordinates(geometry)[, 2])]

  load_particles_t0 <- load_particles[time == min(t0), .(X = mean(X), Y = mean(Y))]
  load_particles_t0 <- load_particles_t0[rep(1:.N, each = n_id)]
  load_particles_t0[, `:=`(id = .I - 1, time = min(t0), dispersaltime = 0)]

  # Add missing 'geometry' column to load_particles_t0
  load_particles_t0[, geometry := NA_character_]
  class(load_particles_t0$geometry) <- class(load_particles$geometry)


  # Bind original particles with new t0
  particle_points <- rbindlist(list(load_particles[time > t0], load_particles_t0), use.names = TRUE, fill = TRUE)
  particle_points <- particle_points[order(id, dispersaltime)]



  ############ 2. competency ####



  n_id <- length(levels(as.factor(particle_points$id)))

  if (competency.function == "exponential") {
    dataset_quartiles <- foreach::foreach(i=1:n_sims, .combine="rbind") %do% {
      post_sm1_sample_exp <- parameter_draws_exp %>% slice_sample(n = n_sims)
      individual_times <- rexp(runif(n_id), rate = 1/(exp(post_sm1_sample_exp[1,1])))
      data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i))
    }

    dataset_quartiles <- dataset_quartiles |> dplyr::mutate(sim=as.factor(sim))
    simulated_settlers <- dataset_quartiles |> dplyr::filter(sim %in% sample(1:n_sims,1)) |> dplyr::select(-sim) |> arrange(id)

  } else if (competency.function == "lognormal") {
    dataset_quartiles <- foreach::foreach(i=1:n_sims, .combine="rbind") %do% {
      post_sm1_sample <- parameter_draws_log %>% slice_sample(n = n_sims)
      individual_times <-  rlnorm(runif(n_id), meanlog=post_sm1_sample[1,1], sdlog=post_sm1_sample[1,2])
      data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i))
    }

    simulated_settlers <- dataset_quartiles |> dplyr::filter(sim %in% sample(1:n_sims,1)) |> dplyr::select(-sim) |> arrange(id)
    dataset_quartiles <- dataset_quartiles |> dplyr::mutate(sim=as.factor(sim))


  } else if (competency.function == "weibull") {
    dataset_quartiles <- foreach::foreach(i=1:n_sims, .combine="rbind") %do% {
      post_sm1_sample <- parameter_draws_weibull %>% slice_sample(n = n_sims)
      individual_times <- rweibull(runif(1000), shape = post_sm1_sample[1,2], scale = post_sm1_sample[1,1])
      data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i))
    }

    simulated_settlers <- dataset_quartiles |> dplyr::filter(sim %in% sample(1:n_sims,1)) |> dplyr::select(-sim) |> arrange(id)
    dataset_quartiles <- dataset_quartiles |> dplyr::mutate(sim=as.factor(sim))

  } else {
    cat("competency.function must be one of lognormal, exponential, weibull")
  }
  competency_times <- simulated_settlers |>
    with(simulated_settlers) |>
    dplyr::mutate(id = (unique(as.factor(particle_points$id))))


  all_times <- particle_points_dt[, .(start_time = min(time), end_time = max(time)), by = id]
  all_times <- CJ(id = all_times$id, time = seq(min(all_times$start_time), max(all_times$end_time), by = "1 min"))
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

  #particle_points_expanded <- sf::st_as_sf(particle_points_dt, coords = c("X", "Y"))
 # particle_points_dt_expanded <- particle_points_dt


###### 3. mortality ####



   n_mortality <- length(levels(unique(as.factor(particle_points_dt$id)))) * simulate.mortality.n
  dead_id_levels <- sample(levels(particle_points_dt$id), n_mortality)
  n_id <- length(unique(particle_points_dt$id))

  # Subset dead particles
  df_dead <- particle_points_dt[id %in% dead_id_levels, na.omit(.SD)]

  # Keep live particles
  df_alive <- particle_points_dt[!id %in% dead_id_levels, na.omit(.SD)]
  df_alive[, c("geometry") := NULL]

  # Survivorship function
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
  survivorship_output <- data.table(
    type = rep(c("typeI", "typeII", "typeIII"), each = n_mortality),
    mortalitytime = c(typeI_time, typeII_time, typeIII_time),
    id = sample(dead_id_levels)
  )[type == simulate.mortality]

  df_dead_timed <- df_dead[survivorship_output, on = "id"][dispersaltime <= mortalitytime]
  df_dead_timed[, c("mortalitytime", "type", "geometry") := NULL]
  df_dead_timed[, id := as.factor(id)]
  particle_points_expanded_postmortality <- rbindlist(list(df_alive, df_dead_timed))

  particle_points_expanded_postmortality <- st_as_sf(particle_points_expanded_postmortality, coords = c("X", "Y"), crs = 20353)




  ############ 3. Settle particles by probability ####


    # bind with benthic substrates
  particle_points_probability <- sf::st_join(particle_points_expanded_postmortality, seascape |> dplyr::mutate(class=as.factor(class)))  |>
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
      dplyr::left_join(select_particles |> as.data.frame() |> select(id, dispersaltime) |> rename(maxdispersaltime=dispersaltime), by="id") |>
      dplyr::filter(dispersaltime < maxdispersaltime) |> # dplyr::select settled particles
      dplyr::arrange(id, time) |>
      dplyr::group_by(id) |>
      dplyr::filter(dplyr::n() >= 3) |> #from particles_to_tracks
      dplyr::group_by(id) |> #from particles_to_tracks
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
