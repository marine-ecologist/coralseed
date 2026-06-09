#' Predict competency
#'
#' Function to generate time-to-settlement probability distributions using a `brms` model
#'
#' Examples:
#' predict_competency(infamis_tiles_exp, n_particles = 100)
#' predict_competency(infamis_tiles_weibull, n_particles = 100)
#' predict_competency(infamis_tiles_log, n_particles = 100)
#'
#' @param input brmsfit object from a model using 'lognormal', 'exponential', or 'weibull' family
#' @param n_particles number of random simulations
#' @param max.time optional maximum dispersal time limit (default is max time in input$data)
#' @param seed.value random seed for reproducibility
#' @param return.plot if TRUE, returns ggplot of survival curves + point estimates
#' @param ... additional arguments (not used)
#' @export

predict_competency <- function(input, n_particles = 100, max.time = NULL,
                               seed.value = NULL, return.plot = FALSE, ...) {

  set.seed(seed.value)
  brms_family <- input$family$family
  model_draws <- brms::as_draws_df(input)

  if (is.null(max.time)) {
    max.time <- max(input$data$time, na.rm = TRUE)
  }

  if (brms_family == "exponential") {
    simulate_curves <- purrr::map_dfr(1:n_particles, function(i) {
      draw <- dplyr::slice_sample(model_draws, n = 1)
      lambda <- exp(-draw$b_Intercept)
      minutes <- min(floor(rexp(1, rate = lambda)), max.time)
      competency_probability <- 1 - pexp(minutes, rate = lambda)
      data.frame(id = i - 1, minutes = minutes, lambda = lambda,
                 b_Intercept = draw$b_Intercept,
                 competency_probability = competency_probability)
    })
    gc()

  } else if (brms_family == "weibull") {
    simulate_curves <- purrr::map_dfr(1:n_particles, function(i) {
      draw <- dplyr::slice_sample(model_draws, n = 1)
      shape <- draw$shape
      scale <- exp(draw$b_Intercept)
      minutes_raw <- scale * (-log(1 - runif(1)))^(1 / shape)
      minutes <- min(floor(minutes_raw), max.time)
      competency_probability <- 1 - pweibull(minutes, shape, scale)
      data.frame(id = i - 1, minutes = minutes,
                 shape = shape, scale = scale,
                 competency_probability = competency_probability)
    })
    gc()

  } else if (brms_family == "lognormal") {
    simulate_curves <- purrr::map_dfr(1:n_particles, function(i) {
      draw <- dplyr::slice_sample(model_draws, n = 1)
      meanlog <- draw$b_Intercept
      sdlog <- draw$sigma
      minutes_raw <- rlnorm(1, meanlog, sdlog)
      minutes <- min(floor(minutes_raw), max.time)
      competency_probability <- 1 - plnorm(minutes, meanlog, sdlog)
      data.frame(id = i - 1, minutes = minutes,
                 meanlog = meanlog, sdlog = sdlog,
                 competency_probability = competency_probability)
    })
    gc()

  } else {
    stop("Unsupported family: must be one of 'exponential', 'weibull', or 'lognormal'")
  }

  minutes_seq <- seq(from = 1, to = max.time, by = max.time / 30)

  predicted_curves <- simulate_curves %>%
    dplyr::group_by(id) %>%
    dplyr::do({
      if (brms_family == "exponential") {
        competency_probability <- 1 - pexp(minutes_seq, rate = .$lambda)
      } else if (brms_family == "weibull") {
        competency_probability <- 1 - pweibull(minutes_seq, shape = .$shape, scale = .$scale)
      } else if (brms_family == "lognormal") {
        competency_probability <- 1 - plnorm(minutes_seq, meanlog = .$meanlog, sdlog = .$sdlog)
      }
      data.frame(minutes = minutes_seq, competency_probability = competency_probability)
    })

  predicted_median <- predicted_curves %>%
    dplyr::group_by(minutes) %>%
    dplyr::summarise(competency_probability = median(competency_probability), .groups = "drop")

  simulated_settlers <- simulate_curves %>%
    dplyr::select(minutes, id) %>%
    dplyr::rename(settlement_point = minutes)

  if (return.plot) {
    plot <- ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::geom_line(data = predicted_curves,
                         ggplot2::aes(x = minutes, y = competency_probability, group = id),
                         color = "turquoise4", alpha = 0.1) +
      ggplot2::geom_line(data = predicted_median,
                         ggplot2::aes(x = minutes, y = competency_probability),
                         color = "black") +
      ggplot2::geom_point(data = simulate_curves %>% dplyr::filter(minutes < max.time),
                          ggplot2::aes(x = minutes, y = competency_probability),
                          color = "black", fill = "coral", shape = 21, stroke = 0.3, size = 2) +
      ggplot2::geom_vline(xintercept = 12, alpha = 0.5) +
      ggplot2::labs(title = paste0(toupper(brms_family), " time-to-settlement curves"),
                    x = "Time to Competency (minutes)",
                    y = "Probability") +
      ggplot2::ylim(0, 1) +
      ggplot2::scale_x_continuous(limits = c(0, max.time), breaks = scales::pretty_breaks(n = 6))

    return(list(simulated_settlers_plot = plot,
                simulated_settlers = simulated_settlers))
  } else {
    return(list(simulated_settlers = simulated_settlers))
  }
}
