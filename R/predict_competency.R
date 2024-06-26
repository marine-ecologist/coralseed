#' Predict_competency
#'
#' Function to generate probability distribution from Allen Coral Atlas input files
#'  predict_competency(n_id=100, nsims=1000, competency.function="exponential")
#' @export
#' @param n_id input n_id (number of particles)
#' @param n_sims number of sims for randomised datasets (see plot for traces)
#' @param competency.function distribution, one of "weibull", "exponential", "lognormal"
#' @param sort sort output by ID, otherwise if FALSE randomly distribute
#' @param seed.value set seed value, defaults to NULL
#' @param return.plot return output
#' @param ... passes functions
#' @export

#'
#'
predict_competency <- function(n_id, n_sims=1000, competency.function = "exponential", b_Intercept=b_Intercept, sort = TRUE, seed.value = NULL, return.plot = TRUE, ...) {

  set.seed(seed.value)
  # for each individual random draw from function
  data(parameter_draws_log, envir = environment())
  data(parameter_draws_exp, envir = environment())
  data(parameter_draws_weibull, envir = environment())

  if (competency.function == "exponential") {

    if (!is.null(b_Intercept)){
      b_Intercept_variance <- sd(coralseed::parameter_draws_exp$b_Intercept)
      sim_exp <- data.frame(b_Intercept = rnorm(1000, b_Intercept, b_Intercept_variance))

      dataset_quartiles <- foreach::foreach(i=1:n_sims, .combine="rbind") %do% {
        post_sm1_sample_exp <- sim_exp %>% dplyr::slice_sample(n = n_sims)
        individual_times <- rexp(runif(n_id), rate = 1/(exp(post_sm1_sample_exp[1,1])))
        data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i))
      }

      simulated_settlers <- dataset_quartiles |> dplyr::filter(sim %in% sample(1:n_sims,1)) |> dplyr::select(-sim) |> arrange(id)

    } else {
        dataset_quartiles <- foreach::foreach(i=1:n_sims, .combine="rbind") %do% {
          post_sm1_sample_exp <- parameter_draws_exp %>% dplyr::slice_sample(n = n_sims)
          individual_times <- rexp(runif(n_id), rate = 1/(exp(post_sm1_sample_exp[1,1])))
          data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i))
        }

        simulated_settlers <- dataset_quartiles |> dplyr::filter(sim %in% sample(1:n_sims,1)) |> dplyr::select(-sim) |> arrange(id)
    }



  } else if (competency.function == "lognormal") {
    dataset_quartiles <- foreach::foreach(i=1:n_sims, .combine="rbind") %do% {
      post_sm1_sample <- parameter_draws_log %>% dplyr::slice_sample(n = n_sims)
      individual_times <-  rlnorm(runif(n_id), meanlog=post_sm1_sample[1,1], sdlog=post_sm1_sample[1,2])
      data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i))
    }


     simulated_settlers <- dataset_quartiles |> dplyr::filter(sim %in% sample(1:n_sims,1)) |> dplyr::select(-sim) |> arrange(id)

  } else if (competency.function == "weibull") {
    dataset_quartiles <- foreach::foreach(i=1:n_sims, .combine="rbind") %do% {
      post_sm1_sample <- parameter_draws_weibull %>% dplyr::slice_sample(n = n_sims)
      individual_times <- rweibull(runif(1000), shape = post_sm1_sample[1,2], scale = post_sm1_sample[1,1])
      data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i))
    }

     simulated_settlers <- dataset_quartiles |> dplyr::filter(sim %in% sample(1:n_sims,1)) |> dplyr::select(-sim) |> arrange(id)
     dataset_quartiles <- dataset_quartiles |> dplyr::mutate(sim=as.factor(sim))

  } else {
     cat("competency.function must be one of logarithmic, exponential, weibull")
  }

  if (sort == TRUE) {
    # re-sort by time and add new sequential IDs
    simulated_settlers <- simulated_settlers |>
      dplyr::arrange(settlement_point) |>
      dplyr::mutate(id = as.factor(rev(seq(0, n_id - 1, 1))))
  } else {
    simulated_settlers <- simulated_settlers |>
      dplyr::arrange(settlement_point) |>
      dplyr::mutate(id = as.factor(sample(seq(0, n_id - 1, 1))))
  }

  if (return.plot == TRUE) {

   plot <- ggplot() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste0("1. Predicted competency (",competency.function, " n=", length(unique(simulated_settlers$id)),")")) +
      ggplot2::xlim(0, 720/60) +
      ggplot2::geom_point(data = dataset_quartiles, ggplot2::aes(settlement_point/60, as.numeric(id), group=sim), color="lightblue", size=0.1, alpha=0.6) +
      ggplot2::geom_point(data = simulated_settlers, ggplot2::aes(settlement_point/60, as.numeric(id)), colour="black", size=1, alpha=0.8) +
      ggplot2::xlab("Time till competent following release (hrs)") +
      ggplot2::ylab("Number of individuals")



     return_list <- list(plot, simulated_settlers)

    names(return_list) <- c("simulated_settlers_plot", "simulated_settlers")
    return(return_list)

  } else if (return.plot == FALSE) {
    return_list <- list(simulated_settlers)
    names(return_list) <- c("simulated_settlers")
    return(return_list)
  }
}
