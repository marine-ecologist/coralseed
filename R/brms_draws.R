#' return draws
#'
#' Function to return draws from time-to-competency brms model
#'
#' @param input brms model fit
#' @param n number of simulations
#' @param tmax max time in hours
#' @param by seq by in minutes
#' @export
#'

return_draws <- function(input, fit, tmax=12, by=1, n=1000, ...){


  if (fit=="exp"){

    # Adapt to exponential model
    simulated_data <- map_dfr(1:n, function(i) {
      draw <- brms::as_draws_df(input) |> slice_sample(n=1)

      b_Intercept <- draw$b_Intercept
      lambda <- exp(-b_Intercept)  # Exponential rate parameter (lambda)
      minutes <- rexp(1, rate = lambda)
      competency_probability <- exp(-lambda * minutes)
      data.frame(id = i,
                 minutes = minutes,
                 lambda = lambda,
                 b_Intercept = b_Intercept,
                 competency_probability = competency_probability)
    })



    # Time range for the exponential survival curve
    minutes <- seq(1, max(simulated_data$minutes), 1)

    # Calculate survival probabilities distribution (curves) for each id
    predicted_curves <- simulated_data %>%
      group_by(id) %>%
      do(data.frame(minutes = minutes,
                    competency_probability = exp(-.$lambda * minutes))) %>%
      left_join(., simulated_data |>
                  dplyr::filter(minutes <= max(minutes)) |>
                  select(id, minutes) |>
                  rename(competency_point=minutes), by="id")

    return(predicted_curves)


  } else if (fit=="weibull"){


    simulated_data <- map_dfr(1:n, function(i) {
      draw <- brms::as_draws_df(input) |> slice_sample(n=1)

      b_Intercept <- draw$b_Intercept
      shape <- draw$shape
      scale <- exp(b_Intercept)  # Scale parameter

      minutes <- scale * (-log(1 - runif(1)))^(1 / shape) # Calculate the time to competency
      competency_probability <- 1 - pweibull(minutes, shape, scale) # Calculate the competency probability at the time to competency

      data.frame(id = i,
                 minutes = minutes,
                 shape = shape,
                 scale = scale,
                 b_Intercept = b_Intercept,
                 competency_probability = competency_probability)
    })


    # Time range for the exponential survival curve
    minutes <- seq(1, max(simulated_data$minutes), 1)

    # Calculate survival probabilities distribution (curves) for each id
    predicted_curves <- simulated_data %>%
      group_by(id) %>%
      do(data.frame(minutes = minutes,
                    competency_probability = 1 - pweibull(minutes, .$shape, exp(.$b_Intercept)))) %>%
      left_join(., simulated_data |>
                  dplyr::filter(minutes <= max(minutes)) |>
                  select(id, minutes) |>
                  rename(competency_point=minutes), by="id")

    return(predicted_curves)


  } else if (fit=="log"){


    # Adapt to lognormal model
    simulated_data <- map_dfr(1:n, function(i) {
      draw <- brms::as_draws_df(input) |> slice_sample(n=1)

      b_Intercept <- draw$b_Intercept
      sigma <- draw$sigma
      mu <- b_Intercept  # In log-normal, mu is the mean of the log of the variable

      # Calculate the time to competency based on the lognormal model
      minutes <- exp(mu + sigma * rnorm(1))

      # Calculate the competency probability at the time to competency
      competency_probability <- 1 - plnorm(minutes, meanlog = mu, sdlog = sigma)

      data.frame(id = i,
                 minutes = minutes,
                 sigma = sigma,
                 mu = mu,
                 b_Intercept = b_Intercept,
                 competency_probability = competency_probability)
    })



    # Time range for the exponential survival curve
    minutes <- seq(1, max(simulated_data$minutes), 1)

    # Calculate survival probabilities distribution (curves) for each id
    predicted_curves <- simulated_data %>%
      group_by(id) %>%
      do(data.frame(minutes = minutes,
                    competency_probability = 1 - plnorm(minutes, meanlog = .$mu, sdlog = .$sigma))) %>%
      left_join(., simulated_data |>
                  dplyr::filter(minutes <= max(minutes)) |>
                  select(id, minutes) |>
                  rename(competency_point=minutes), by="id")

    return(predicted_curves)

  } else {
    print("Error: fit should be one of `log`, `exp`, `weibull`")
  }



}

#' plot draws
#'
#' Function to plot spaghetti draws from time-to-competency brms model
#'
#' @param n number of simulations
#' @param slice n number of simulations for spaghetti plot
#' @export
#'

plot_draws <- function(input, slice=100){

    dataset_grouped_subset <- input %>% dplyr::group_by(id) %>% dplyr::slice_sample(n=slice)
    dataset_grouped_median <- input %>% dplyr::group_by(minutes) %>% dplyr::summarise(competency_point=median(competency_point))

    # Plot
  plot <- ggplot2::ggplot() + ggplot2::theme_bw() + ggplot2::xlab("Hours after release") + ggplot2::ylab("Settlement Probability") + ggplot2::ylim(0,1) +
    ggplot2::geom_line(data=dataset_grouped_subset, ggplot2::aes(x=minutes/60, y=competency_point, group=id), linewidth=0.05, color="slategrey") +
    ggplot2::geom_line(data=dataset_grouped_median,ggplot2:: aes(x=minutes/60, y=competency_point), linewidth=1, color="black")

  return(plot)

}



#' plot draws
#'
#' Function to plot median and 90% CI draws from time-to-competency brms model
#'
#' @param n number of simulations
#' @export
#'

plot_draws_median <- function(input){

  # Calculate 2.5th and 97.5th percentiles
  dataset_grouped_quantiles <- input %>%
    dplyr::group_by(minutes) %>%
    dplyr::summarise(
      lower95 = quantile(competency_point, probs = 0.025),
      upper95 = quantile(competency_point, probs = 0.975),
      lower90 = quantile(competency_point, probs = 0.05),
      upper90 = quantile(competency_point, probs = 0.95),

      median = median(competency_point)
    )

  # Plot with 95% credibility intervals
  plot <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::xlab("Hours after release") +
    ggplot2::ylab("Settlement Probability") +
    ggplot2::ylim(0,1) +
    # geom_line(data=dataset_grouped_subset, aes(x=minutes/60, y=settlement_probability, group=id), linewidth=0.05, color="slategrey") +
    ggplot2::geom_line(data=dataset_grouped_quantiles, ggplot2::aes(x=minutes/60, y=median), linewidth=1, color="black") +
    ggplot2::geom_ribbon(data=dataset_grouped_quantiles, ggplot2::aes(x=minutes/60, ymin=lower90, ymax=upper90), fill="turquoise4", alpha=0.2)

  return(plot)

}
