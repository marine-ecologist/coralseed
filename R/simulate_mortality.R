#' Simulate Mortality
#'
#' Function to simulate mortality of a population. simulate.mortality.n is the proportion of the population to simulate mortality over a 24hr period.
#'
#' Currently set to mortality types I,II,III parameterised from the weibull distribution (return.plot=TRUE to visualise results against 1000 simulations)
#'
#' tmp <- simulate_mortality(particle_points_expanded, simulate.mortality="typeIII",  simulate.mortality.n = 0.2, return.plot=TRUE)
#'
#' @param input input (defaults to particle_points_expanded from seed_particles() function)
#' @param simulate.mortality select which type of mortality (one of "typeI", "typeII", "typeIII")
#' @param simulate.mortality.n proportion of the population to kill (0 = none, 1 = 100% mortality)
#' @param return.plot show plot of the results (defaults to FALSE)
#' @param silent silence printing results while running (defaults to FALSE)
#' @param set.seed set seed for consistent results (defaults to NULL)
#' @param ... pass arguments
#' @export
#'

simulate_mortality <- function(
    input, simulate.mortality = NULL, simulate.mortality.n = 0.1, # change to %?
    return.plot = TRUE, silent = FALSE, set.seed = NULL, ...) {
  
  if (simulate.mortality == "none") {
  
    plot0 <- ggplot2::ggplot() +  
      ggplot2::geom_text(ggplot2::aes(x=0.5, y=0.5, label="no data \n(mortality set to zero)"), size=4, color="darkred", inherit.aes = FALSE) +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) + 
      ggplot2::theme_bw() +
      ggplot2::ggtitle("no mortality selected")

    df.bypass <- input
    
    return_list <- list(plot0, df.bypass)
    
    names(return_list) <- c("simulated_mortality_plot", "simulated_mortality")
    return(return_list)
  
  } else {
    set.seed(set.seed)
    n_mortality <- length(levels(unique(as.factor(input$id)))) * simulate.mortality.n

    dead_id_levels <- sample(levels(input$id), n_mortality)

    n_id <- length(unique(input$id))
    df_dead <- input |>
      dplyr::filter(id %in% dead_id_levels) |>
      tidyr::drop_na(id) # subset dead particles
    df_alive <- input |>
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


    t6 <- (sum(survivorship_output$mortalitytime < 360)) # / n_id) #* 100
    t12 <- (sum(survivorship_output$mortalitytime < 720)) # / n_id) #* 100
    if (silent == FALSE) {
      (cat(paste0("Survivorship curve ", unique(survivorship_output$type), " \n")))
      (cat(paste0("Mortality at t6 = ", t6, " / ", n_id, " larvae \n")))
      (cat(paste0("Mortality at t12 = ", t12, " / ", n_id, " larvae \n")))
    }
    df_joined <- rbind(df_alive, df_dead_timed)
  }


  if (return.plot == TRUE) {
    survivorship_loop <- NULL
    for (i in 1:n_mortality) {
      # Fit the types to the individuals and sample the time of death
      typeI_time <- survivorship_type(n_mortality, 2.5, 1440)
      typeII_time <- survivorship_type(n_mortality, 1.5, 1440)
      typeIII_time <- survivorship_type(n_mortality, 0.5, 1440)

      # Combine into a data frame
      loop_results <- data.frame(
        type = rep(c("typeI", "typeII", "typeIII"), each = n_mortality),
        time = c(typeI_time, typeII_time, typeIII_time)
      ) |>
        dplyr::group_by(type) |>
        dplyr::arrange(type, time) |>
        dplyr::ungroup() |>
        dplyr::mutate(n = rep(seq(1, n_mortality, 1), 3))
      survivorship_loop[[i]] <- loop_results
    }

    survivorship_loop <- do.call(rbind, survivorship_loop)

    survivorship_output_plot <- survivorship_output |>
      dplyr::arrange(mortalitytime) |>
      dplyr::mutate(id = seq(1, n_mortality, 1))

    oldwarning <- getOption("warn")
    options(warn = -1)
    plot1 <- ggplot2::ggplot() +
      ggplot2::theme_bw() + 
      ggplot2::geom_point(data = survivorship_loop, ggplot2::aes(x = time, y = n, color = as.factor(type)), size = 1.5, alpha = 0.02) +
      ggplot2::scale_color_manual(values = viridis::viridis(n = 4)[-4]) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1.0, size = 2))) +
      ggplot2::geom_point(data = survivorship_output_plot, ggplot2::aes(mortalitytime, as.numeric(id)), size = 2, shape = 21, show.legend = FALSE, fill = NA, color = "black") +
      ggplot2::theme(legend.position = c(0.8,0.2), legend.title = element_blank(), legend.background = element_blank()) 
      

    return_list <- list(plot1, df_joined)
    names(return_list) <- c("simulated_mortality_plot", "simulated_mortality")
    return(return_list)
    options(warn=oldwarning)
  } else if (return.plot == FALSE) {
    return_list <- list(df_joined)
    names(return_list) <- c("simulated_mortality")
    return(return_list)
 
  }
}
