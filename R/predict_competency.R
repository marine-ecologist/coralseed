#' Predict_competency
#'
#' Function to generate probability distribution from Allen Coral Atlas input files
#'
#'
#' predict_competency_time(infamis_tiles_exp, n_particles=100)
#' predict_competency_time(infamis_tiles_log, n_particles=100)
#' predict_competency(infamis_tiles_weibull, n_particles=100)
#'
#' @export
#' @param input brmsfit of either 'lognormal', 'exponential', 'weibull' functions
#' @param n_particles number of sims for randomised datasets (see plot for traces)
#' @param seed.value set seed value, defaults to NULL
#' @param return.plot return output
#' @param ... passes functions
#' @export


predict_competency<- function(input, n_particles=100, seed.value=NULL, return.plot=FALSE){

  set.seed(seed.value)
  brms_family <- input$family$family
  model_draws <- brms::as_draws_df(input)


  if(brms_family == "exponential") {

    simulate_curves <- purrr::map_dfr(1:n_particles, function(i) {
      draw <- model_draws |> dplyr::slice_sample(n = 1)

      b_Intercept <- draw$b_Intercept
      lambda <- exp(-b_Intercept)  # Exponential rate parameter (lambda)
      minutes <- rexp(1, rate = lambda)  # Time to competency
      competency_probability <- 1 - pexp(minutes, rate = lambda)  # Competency probability

      data.frame(
        id = i - 1,
        minutes = floor(minutes),
        lambda = lambda,
        b_Intercept = b_Intercept,
        competency_probability = competency_probability
      )
    })

  } else if (brms_family == "weibull") {

    simulate_curves <- purrr::map_dfr(1:n_particles, function(i) {
      draw <- model_draws |> dplyr::slice_sample(n = 1)

      shape <- draw$shape  # Shape parameter
      scale <- exp(draw$b_Intercept)  # Scale parameter
      minutes <- scale * (-log(1 - runif(1)))^(1 / shape)  # Time to competency
      competency_probability <- 1 - pweibull(minutes, shape, scale)  # Competency probability

      data.frame(
        id = i - 1,
        minutes = floor(minutes),
        shape = shape,
        scale = scale,
        competency_probability = competency_probability
      )
    })

  } else if (brms_family == "lognormal") {

    simulate_curves <- purrr::map_dfr(1:n_particles, function(i) {
      draw <- model_draws |> dplyr::slice_sample(n = 1)

    meanlog <- draw$b_Intercept
    sdlog <- draw$sigma

    minutes <- rlnorm(1, meanlog, sdlog)  # Time to competency
    competency_probability <- 1 - plnorm(minutes, meanlog, sdlog)  # Competency probability

      data.frame(
        id = i - 1,
        minutes = floor(minutes),
        meanlog = meanlog,
        sdlog = sdlog,
        competency_probability = competency_probability
      )
    })

  } else {
    cat("Input must be a brms model fit from either exponential, weibull, or lognormal family")
  }

  # Time range for the survival curve
  minutes <- seq(1, max(simulate_curves$minutes), 1)

  # Calculate survival probabilities distribution (curves) for each id
  predicted_curves <- simulate_curves %>%
    dplyr::group_by(id) %>%
    dplyr::do({
      minutes <- seq(1, max(simulate_curves$minutes), 1)

      if (brms_family == "exponential") {
        competency_probability <- 1 - pexp(minutes, rate = .$lambda)
      } else if (brms_family == "weibull") {
        competency_probability <- 1 - pweibull(minutes, .$shape, .$scale)
      } else if (brms_family == "lognormal") {
        competency_probability <- 1 - plnorm(minutes, meanlog = .$meanlog, sdlog = .$sdlog)
      } else {
        stop("Unknown family")
      }

      data.frame(minutes = minutes, competency_probability = competency_probability)
    })

  predicted_median <- predicted_curves %>%
    dplyr::group_by(minutes) %>%
    dplyr::summarise(competency_probability = median(competency_probability))

  predicted_point <- simulate_curves %>%
    dplyr::filter(minutes <= max(minutes)) %>%
    dplyr::mutate(id = id - 1)

  simulated_settlers <- simulate_curves %>%
    dplyr::select(minutes, id) %>%
    dplyr::rename(settlement_point = minutes)



  if (return.plot == TRUE) {

    # Predicted survival curves and points
    plot <- ggplot2::ggplot() + ggplot2::theme_bw() + ggplot2::xlim(0,48) +
      ggplot2::geom_line(data = predicted_curves, ggplot2::aes(x = minutes/60, y = competency_probability, group = id), color = "turquoise4", alpha = 0.1) +
      ggplot2::geom_line(data = predicted_median, ggplot2::aes(x = minutes/60, y = competency_probability), color = "black", alpha = 1.5) +
      ggplot2::geom_vline(xintercept = 12, alpha = 0.5) +
      ggplot2::geom_point(data = simulate_curves, ggplot2::aes(x = minutes/60, y = competency_probability), color = "black", fill = "coral", shape = 21, stroke = 0.3, size = 2, alpha = 1) +
      ggplot2::labs(title = paste0("Predicted ", toupper(brms_family), " Survival Curves"),
           x = "Time to Competency (hours)",
           y = "Probability")


    return_list <- list(plot, simulated_settlers)

    names(return_list) <- c("simulated_settlers_plot", "simulated_settlers")
    return(return_list)

  } else if (return.plot == FALSE) {
    return_list <- list(simulated_settlers)
    names(return_list) <- c("simulated_settlers")
    return(return_list)
  }
}

