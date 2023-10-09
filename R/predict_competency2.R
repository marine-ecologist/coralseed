#' Predict_competency
#'
#' Function to generate probability distribution from Allen Coral Atlas input files
#'
#' @export
#' @param n_id input n_id
#' @param competency.function distribution, one of "weibull", "exp", "log"
#' @param sort sort output by ID, otherwise if FALSE randomly distribute
#' @param set.seed set seed value, defaults to NULL
#' @param return.plot return output
#' @param ... passes functions
#'

predict_competency2 <- function(n_id, competency.function = "exponential", sort = FALSE, set.seed = NULL, return.plot = TRUE, ...) {
  # for each individual random draw from function

  if (competency.function == "exponential") {
    set.seed(NULL)
    posterior_draws <- parameter_draws_exp
    draw_individuals <- function(row) {
      rexp(1, rate = 1 / exp(row["b_Intercept"]))
    }
    random_draws <- posterior_draws |> dplyr::slice_sample(n = n_id)
    all_samples <- apply(random_draws, 1, draw_individuals)
    simulated_settlers <- data.frame(settlement_point = ceiling(all_samples)) |>
      dplyr::mutate(id = as.factor(rev(seq(0, n_id - 1, 1))))

  } else if (competency.function == "logarithmic") {
    set.seed(set.seed)
    posterior_draws <- parameter_draws_log
    draw_individuals <- function(row) {
      rlnorm(1, row["b_Intercept"])
    }
    random_draws <- posterior_draws |> dplyr::slice_sample(n = n_id)
    all_samples <- apply(random_draws, 1, draw_individuals)
    simulated_settlers <- data.frame(settlement_point = ceiling(all_samples)) |>
      dplyr::mutate(id = as.factor(rev(seq(0, n_id - 1, 1))))

  } else if (competency.function == "weibull") {
    set.seed(set.seed)
    posterior_draws <- parameter_draws_weibull
    draw_individuals <- function(row) {
      rweibull(1, shape = row["shape"], scale = row["scale"])
    }
    random_draws <- posterior_draws |> dplyr::slice_sample(n = n_id)
    all_samples <- apply(random_draws, 1, draw_individuals)

    simulated_settlers <- data.frame(settlement_point = ceiling(all_samples)) |>
      dplyr::mutate(id = as.factor(rev(seq(0, n_id - 1, 1))))
  } else {
    cat("competency.function must be one of 'logarithmic', 'exponential', 'weibull'")
  }
  # random_draws <- posterior_draws |> dplyr::slice_sample(n = n_id) # randomly sample posterior draws to meet n
  # all_samples <- apply(random_draws, 1, draw_individuals) # apply the draw_individuals function across rows of random_draws

  if (sort == TRUE) {
    # re-sort by time and add new sequential IDs
    simulated_settlers <- simulated_settlers |>
      dplyr::arrange(settlement_point) |>
      dplyr::mutate(id = as.factor(rev(seq(0, n_id - 1, 1))))
  }

  if (return.plot == TRUE) {
    simulated_settlers_plot <- simulated_settlers |>
      dplyr::arrange(settlement_point) |>
      dplyr::mutate(id = as.factor(rev(seq(0, n_id - 1, 1))))
    oldwarning <- getOption("warn")
    options(warn = -1)
    plot <- ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste0("1. Predicted competency (n=", length(unique(simulated_settlers$id)), " , ", competency.function, " distribution)")) +
      ggplot2::xlim(0, 1440 / 60) +
      ggplot2::geom_point(data = simulated_settlers_plot, ggplot2::aes(settlement_point / 60, as.numeric(id)), size = 0.1) +
      ggplot2::xlab("Timing of competency (hrs)") +
      ggplot2::ylab("Individual")
    options(warn = oldwarning)

    return_list <- list(plot, simulated_settlers)
    names(return_list) <- c("simulated_settlers_plot", "simulated_settlers")
    return(return_list)
  } else if (return.plot == FALSE) {
    return_list <- list(simulated_settlers)
    names(return_list) <- c("simulated_settlers")
    return(return_list)
  }
}
