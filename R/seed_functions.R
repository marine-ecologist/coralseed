
###
simulate_larvae <- function (n_sims, n_id, b_intercept, n_seed){

  set.seed(n_seed) # seed
  n_id = n_id # number of larvae
  n_sims=n_sims # how many simulations to draw across
  b_Intercept=b_intercept # shape of the curve

  b_Intercept_variance <- sd(coralseed::parameter_draws_exp$b_Intercept)
  sim_exp <- data.frame(b_Intercept = rnorm(1000, b_Intercept, b_Intercept_variance))

  dataset_quartiles <- foreach::foreach(i=1:n_sims, .combine="rbind") %do% {
    post_sm1_sample_exp <- sim_exp %>% dplyr::slice_sample(n = n_sims)
    post_sm1_sample_exp <- sim_exp %>% dplyr::slice_sample(n = n_sims)
    individual_times <- rexp(runif(n_id), rate = 1/(exp(post_sm1_sample_exp[1,1])))
    data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i))
  }

  simulated_settlers_total <- dataset_quartiles |>
    dplyr::filter(sim %in%  "101") |>  # sample(1:n_sims,1)) |> # randomly sample adplyr:: filter? fixing for now
    dplyr::select(-sim) |>
    arrange(settlement_point) %>%
    mutate(id=seq(1:nrow(.)))


  return(simulated_settlers_total)


  ## original function below for storage if reverting to parameter draws exponential
  # set.seed(n_seed)
  # predict_exp <- foreach(i=1:nsims, .combine="rbind") %do% {
  #   set.seed(1)
  #   post_sm1_sample_exp <- coralseed::parameter_draws_exp %>% slice_sample(n = 1)
  #   individual_times <- rexp(runif(n_id), rate = 1/(exp(post_sm1_sample_exp[1,1])))
  #   data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i))
  # }
  #
  # simulated_settlers_model <- predict_exp |>
  #   dplyr::filter(sim %in% sample(1:nsims, 1)) |>
  #   select(-sim) %>%
  #   mutate(id=seq(1:n()))
  #
  # return(simulated_settlers_model)
}
initialize_particles <- function(x0, y0, n_particles, settlement_time=NULL) {
  output <- tibble(
    id = 1:n_particles,
    x = rep(x0, n_particles),
    y = rep(y0, n_particles)
  )
  if (!is.null(time)){
    output$settlement_point = settlement_time$settlement_point
  }
  return(output)
}
update_particle_states <- function(particles, velocities, uncertainty_sd, settlers, n_offset=1, e_offset=1) {
  particle_states <- list()
  current_particles <- particles

  for (i in 1:nrow(velocities)) {
    current_particles <- current_particles %>%
      mutate(
        x = (x + (((velocities$velocity_E[i]/100)*60)*e_offset) + rnorm(n(), mean = 0, sd = abs((velocities$velocity_E[i]/100)*60)*uncertainty_sd)),
        y = (y + (((velocities$velocity_N[i]/100)*60)*n_offset) + rnorm(n(), mean = 0, sd = abs((velocities$velocity_N[i]/100)*60)*uncertainty_sd)),
        time = ymd_hms(velocities$time[i])
      )
    particle_states[[i]] <- current_particles
  }

  # combine with settlement predictions
  combined_particles <- bind_rows(particle_states) |>
    left_join(settlers |> select(-time), by="id") |>
    mutate(settlement_time = min(as.POSIXct(time)) + minutes(settlement_point)) |>
    group_by(id) |>
    mutate(state = if_else(time >= settlement_time, 1L, 0L)) |>
    mutate(competency = if_else(state==1, "competent", "incompetent")) |>
    ungroup() |>
    select(-settlement_time) |>
    arrange(id, time) |>
    rename(dispersaltime = settlement_point) |>
    st_as_sf(coords = c("x", "y"), crs = 20353)

  return(combined_particles)
}
plot_particle_states <- function(particles, n=100, t=30){
  particles |>
    dplyr::filter(id %in% sample(unique(particles$id), n)) |>
    group_by(id) |>
    dplyr::filter(row_number() %% t == 0) %>%
    ungroup()

}


seed_particles_interventions <- function(particles, velocities, uncertainty_sd, settlers, option) {



  # Ensure velocities are ordered by time
  velocities <- velocities %>% arrange(time)

  if (option=="seedbox"){
  # Filter particles based on the time condition
  particles <- particles %>%
    mutate(time = min(velocities$time) + minutes(settlement_point)) |>
    dplyr::filter(time < max(velocities$time))
  } else if (option=="pointsource"){
    # filter to ONLY particles that settle, change this later if tracks and full data are needed
    particles <- particles %>%
      mutate(settlement_time=min(velocities$time) + minutes(settlement_point)) |>
      dplyr::filter(settlement_time < max(velocities$time)) |>
      dplyr::select(-settlement_time) |>
      mutate(time = min(velocities$time)) |>
      dplyr::filter(time < max(velocities$time))

  }

#   # Initialize an empty tibble for the final combined particle states
#   combined_particles <- tibble()
#
#   # Iterate over each unique particle id
#   for (pid in unique(particles$id)) {
#     # Filter the particles data frame for the current pid
#     current_particle <- dplyr::filter(particles, id == pid)
#
#     # Find the start time for the current particle and match it with velocities time
#     start_time <- min(current_particle$time)
#     start_index <- which(velocities$time >= start_time)[1]
#
#     # Initialize an empty tibble to accumulate updates for the current pid
#     pid_updates <- tibble()
#
#     # If start_index is found, proceed with updating positions
#     if (!is.na(start_index)) {
#       for (i in start_index:nrow(velocities)) {
#         current_particle <- current_particle %>%
#           mutate(
#             x = x + (velocities$velocity_E[i] / 100)*60 + rnorm(1, mean = 0, sd = abs(velocities$velocity_E[i] / 100)*60 * uncertainty_sd * 12),
#             y = y + (velocities$velocity_N[i] / 100)*60 + rnorm(1, mean = 0, sd = abs(velocities$velocity_N[i] / 100)*60 * uncertainty_sd * 12),
#             time = velocities$time[i]
#           )
#
#         # Accumulate updates for the current pid
#         pid_updates <- rbind(pid_updates, current_particle)
#       }
#     }
#     print(paste0("parsing ", pid))
#
#     # Combine the updates for the current pid into the final combined_particles data frame
#     combined_particles <- rbind(combined_particles, pid_updates) #|>
# #      mutate(settlement_time=min(velocities$time) + minutes(settlement_point))
#   }


  cl <- makeCluster(detectCores() - 1)  # Leave one core free for system processes

  # Register the parallel backend
  registerDoSNOW(cl)

  iterations <- length(unique(particles$id))
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  # Initialize an empty list to store the results from each iteration
  results_list <- list()

  # Use foreach to iterate over each unique particle ID, in parallel
  results_list <- foreach(pid = unique(particles$id), .verbose = TRUE, .combine = 'rbind', .packages = c("dplyr", "lubridate")) %dopar% {
    # Filter the particles data frame for the current pid
    current_particle <- dplyr::filter(particles, id == pid)

    # Find the start time for the current particle and match it with velocities time
    start_time <- min(current_particle$time)
    start_index <- which(velocities$time >= start_time)[1]
    #end_index <-  max(which(velocities$time >= start_time))

    # Initialize an empty tibble to accumulate updates for the current pid
    pid_updates <- tibble()

    # If start_index is found, proceed with updating positions
    if (!is.na(start_index)) {
      for (i in start_index:nrow(velocities)) {
        current_particle <- current_particle %>%
          mutate(
            x = (x + (velocities$velocity_E[i]/100)*60 + rnorm(n(), mean = 0, sd = abs((velocities$velocity_E[i]/100)*60)*uncertainty_sd)) ,
            y = (y + (velocities$velocity_N[i]/100)*60 + rnorm(n(), mean = 0, sd = abs((velocities$velocity_N[i]/100)*60)*uncertainty_sd)),
            time = velocities$time[i]
          )

        # Accumulate updates for the current pid
        pid_updates <- rbind(pid_updates, current_particle)
      }
    }

    return(pid_updates)
  }

  # Convert the results list to a tibble
  combined_particles <- bind_rows(results_list)

  close(pb)
  stopCluster(cl)

  combined_particles2 <- combined_particles |>
   # left_join(settlers |> select(-time), by="id") |>
    mutate(settlement_time =min(velocities$time) + minutes(settlement_point))|>
    group_by(id) |>
    mutate(state = if_else(time >= settlement_time, 1L, 0L)) |>
    mutate(competency = if_else(state==1, "competent", "incompetent")) |>
    ungroup() |>
    select(-settlement_time) |>
    arrange(id, time) |>
    rename(dispersaltime = settlement_point) |>
    st_as_sf(coords = c("x", "y"), crs = 20353)


  return(combined_particles2)
}



settle_particles_interventions <- function(input, seascape=seascape){

  # seed particles component
  particle_states_probability_seedbox <- sf::st_join(input, seascape) |>
    dplyr::mutate(class = forcats::fct_na_value_to_level(class, "Ocean")) |> # replace NA with Ocean
    dplyr::mutate(habitat_id = forcats::fct_na_value_to_level(habitat_id, "Ocean")) |> # replace NA with Ocean
    dplyr::mutate(settlement_probability = tidyr::replace_na(settlement_probability, 0)) |> # make lagoon and ocean 0
    dplyr::mutate(state = as.numeric(as.character(state))) |>
    dplyr::mutate(settlement_outcome = as.numeric(as.character(as.factor(rbinom(length(state), size = 1, prob = settlement_probability))))) |>
    dplyr::mutate(final = (state * settlement_outcome)) |> # turn into probability
    dplyr::mutate(outcome = as.factor(state * settlement_outcome))

  # two functions from settle_particles.R
  particles_states_settled_seedbox <- particle_states_probability_seedbox |>
    dplyr::filter(outcome == "1") |> # dplyr::select settled particles
    dplyr::arrange(id, time) |>
    dplyr::group_by(id, habitat_id) |>
    dplyr::slice_sample(n = 1) |> # randomly sample a point in each habitat where competent
    dplyr::arrange(id, dispersaltime) |>
    dplyr::group_by(id) |>
    dplyr::slice_head(n = 1) |> # take first of the habitats by dispersaltime if multiple intersects
    dplyr::select(id, class, time, dispersaltime) |>
    dplyr::mutate(cat = "settled", cat = as.factor(cat))

  return(particles_states_settled_seedbox)
}

