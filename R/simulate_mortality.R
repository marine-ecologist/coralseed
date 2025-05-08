#' Simulate Mortality
#'
#' Function to simulate mortality of a population. `simulate.mortality.n` is the proportion of the population to simulate mortality over a 24hr period.
#'
#' Mortality types I, II, III are Weibull-distributed. Set `return.plot=TRUE` to visualise simulation vs empirical.
#'
#' Example:
#' tmp <- simulate_mortality(particle_points_expanded, simulate.mortality = "typeIII", simulate.mortality.n = 0.2, return.plot = TRUE)
#'
#' @param input input (e.g. from `seed_particles()`, usually a time-expanded `sf` object)
#' @param simulate.mortality select mortality type ("typeI", "typeII", or "typeIII")
#' @param simulate.mortality.n proportion of population to simulate mortality on (0–1)
#' @param return.plot generate plot output (default: TRUE)
#' @param silent suppress printed messages (default: FALSE)
#' @param seed.value seed for reproducibility (default: NULL)
#' @param ... pass additional args
#' @export
#'

simulate_mortality <- function(
    input = NULL,
    simulate.mortality = NULL,
    simulate.mortality.n = 0.1,
    return.plot = TRUE,
    silent = FALSE,
    seed.value = NULL,
    ...) {

  set.seed(seed.value)

  if (simulate.mortality == "none") {
    plot0 <- ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "no data \n(mortality set to zero)"),
                         size = 4, color = "darkred", inherit.aes = FALSE) +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("no mortality selected")

    return_list <- list(
      simulated_mortality_plot = plot0,
      simulated_mortality = input
    )
    return(return_list)
  }

  n_ids <- length(unique(input$id))
  n_mortality <- n_ids * simulate.mortality.n
  dead_id_levels <- sample(levels(factor(input$id)), n_mortality)

  df_dead <- dplyr::filter(input, id %in% dead_id_levels)
  df_alive <- dplyr::filter(input, !(id %in% dead_id_levels))

  survivorship_type <- function(n, shape, scale) {
    dispersaltime <- seq(1, 1440, 1)
    probs <- dweibull(dispersaltime, shape, scale)
    sample(dispersaltime, size = n, prob = probs, replace = TRUE)
  }

  typeI_time <- survivorship_type(n_mortality, 2.5, 1440)
  typeII_time <- survivorship_type(n_mortality, 1.5, 1440)
  typeIII_time <- survivorship_type(n_mortality, 0.5, 1440)

  survivorship_output <- data.frame(
    type = rep(c("typeI", "typeII", "typeIII"), each = n_mortality),
    mortalitytime = c(typeI_time, typeII_time, typeIII_time),
    id = sample(dead_id_levels)
  ) |>
    dplyr::filter(type == simulate.mortality) |>
    dplyr::mutate(endpoint = 1)

  df_dead_timed <- df_dead |>
    dplyr::left_join(survivorship_output, by = "id") |>
    dplyr::filter(dispersaltime <= mortalitytime) |>
    dplyr::select(-mortalitytime, -type)

  df_joined <- dplyr::bind_rows(
    df_alive |> dplyr::mutate(endpoint = 0),
    df_dead_timed |> dplyr::mutate(endpoint = 1)
  ) |>
    dplyr::select(-endpoint)

  if (!silent) {
    t6 <- sum(survivorship_output$mortalitytime < 360)
    t12 <- sum(survivorship_output$mortalitytime < 720)
    cat(paste0("Survivorship curve ", unique(survivorship_output$type), "\n"))
    cat(paste0("Mortality at t6 = ", t6, " / ", n_ids, " larvae\n"))
    cat(paste0("Mortality at t12 = ", t12, " / ", n_ids, " larvae\n"))
  }

  if (return.plot) {
    plot_df_subset <- dplyr::bind_rows(
      df_alive |> dplyr::mutate(endpoint = 0),
      df_dead_timed |> dplyr::mutate(endpoint = 1)
    ) |>
      dplyr::group_by(id) |>
      dplyr::slice_max(order_by = dispersaltime, n = 1) |>
      dplyr::arrange(endpoint, dispersaltime) |>
      dplyr::ungroup() |>
      dplyr::select(id, dispersaltime, endpoint) |>
      dplyr::mutate(n = dplyr::n() + 1 - dplyr::row_number()) |>
      dplyr::filter(endpoint == 1) |>
      dplyr::mutate(time = ifelse(dispersaltime >= max(dispersaltime), "later", "during")) |>
      dplyr::filter(time == "during")

    plot1 <- ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank()) +
      ggplot2::geom_segment(data = plot_df_subset,
                            ggplot2::aes(x = 0, xend = dispersaltime / 60, y = n, yend = n),
                            color = "black", linewidth = 0.2) +
      ggplot2::geom_point(data = plot_df_subset,
                          ggplot2::aes(x = dispersaltime / 60, y = n, fill = time),
                          size = 2.5, shape = 21) +
      ggplot2::scale_fill_manual(values = c("during" = "darkred")) +
      ggplot2::scale_y_continuous(
        breaks = seq_along(unique(plot_df_subset$id)),
        labels = rev(plot_df_subset$id)
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, max(df_joined$dispersaltime) / 60),
        breaks = scales::pretty_breaks(n = 6)
      ) +
      ggplot2::labs(
        x = "Time after larval release (hours)",
        y = "",
        title = simulate.mortality
      )

    return_list <- list(
      simulated_mortality_plot = plot1,
      simulated_mortality = df_joined,
      survivorship_output = survivorship_output
    )
    return(return_list)
  }

  return(list(simulated_mortality = df_joined))
}



# simulate_mortality <- function(
#     input=NULL, simulate.mortality = NULL, simulate.mortality.n = 0.1, # change to %?
#     return.plot = TRUE, silent = FALSE, seed.value = NULL, ...) {
#
#
#   ### set seed
#
#   set.seed(seed.value)
#   #print(paste0("! is.null seed = ", get(".Random.seed", envir = globalenv())[10]))
#
#   if (simulate.mortality == "none") {
#
#     plot0 <- ggplot2::ggplot() +
#       ggplot2::geom_text(ggplot2::aes(x=0.5, y=0.5, label="no data \n(mortality set to zero)"), size=4, color="darkred", inherit.aes = FALSE) +
#       ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
#       ggplot2::theme_bw() +
#       ggplot2::ggtitle("no mortality selected")
#
#     df.bypass <- input
#
#     return_list <- list(plot0, df.bypass)
#
#     names(return_list) <- c("simulated_mortality_plot", "simulated_mortality")
#     return(return_list)
#
#   } else {
#     #set.seed(set.seed)
#     n_mortality <- length(levels(unique(as.factor(input$id)))) * simulate.mortality.n #%>% round(.,1)
#
#     # sample input ID and assign to dead
#     dead_id_levels <- sample(levels(input$id), n_mortality)
#
#     n_id <- length(unique(input$id))
#     df_dead <- input |>
#       dplyr::filter(id %in% dead_id_levels) |>
#       tidyr::drop_na(id) # subset dead particles
#     df_alive <- input |>
#       dplyr::filter(!(id %in% dead_id_levels)) |>
#       tidyr::drop_na(id) # keep live particles
#
#
#     survivorship_type <- function(n, shape, scale) {
#       dispersaltime <- seq(1, 1440, 1)
#       probabilities <- dweibull(dispersaltime, shape, scale)
#       sample(dispersaltime, size = n, prob = probabilities, replace = TRUE)
#     }
#
#     # Fit the types to the individuals and sample the time of death
#     #set.seed(set.seed)
#     typeI_time <- survivorship_type(n_mortality, 2.5, 1440)
#     typeII_time <- survivorship_type(n_mortality, 1.5, 1440)
#     typeIII_time <- survivorship_type(n_mortality, 0.5, 1440)
#
#     # Combine into a data frame
#     survivorship_output <- data.frame(
#       type = rep(c("typeI", "typeII", "typeIII"), each = n_mortality),
#       mortalitytime = c(typeI_time, typeII_time, typeIII_time),
#       id = sample(dead_id_levels)
#     ) |> dplyr::filter(type == simulate.mortality) |>
#       dplyr::mutate(endpoint = 1)
#
#     df_dead_timed <- df_dead |>
#       dplyr::left_join(survivorship_output, by = "id") |>
#       dplyr::filter(dispersaltime <= mortalitytime) |>
#       dplyr::select(-mortalitytime, -type) |>
#       dplyr::mutate(id = as.factor(id))
#
#
#     t6 <- (sum(survivorship_output$mortalitytime < 360)) # / n_id) #* 100
#     t12 <- (sum(survivorship_output$mortalitytime < 720)) # / n_id) #* 100
#     if (silent == FALSE) {
#       (cat(paste0("Survivorship curve ", unique(survivorship_output$type), " \n")))
#       (cat(paste0("Mortality at t6 = ", t6, " / ", n_id, " larvae \n")))
#       (cat(paste0("Mortality at t12 = ", t12, " / ", n_id, " larvae \n")))
#     }
#
#
#     df_joined <- rbind(df_alive |> mutate(endpoint = 1),
#                        df_dead_timed) |> dplyr::select(-endpoint)
#   }
#
#
#   if (return.plot == TRUE) {
#     survivorship_loop <- NULL
#     for (i in 1:(n_mortality/2)) {
#       # Fit the types to the individuals and sample the time of death
#       typeI_time <- survivorship_type(n_mortality, 2.5, 1440)
#       typeII_time <- survivorship_type(n_mortality, 1.5, 1440)
#       typeIII_time <- survivorship_type(n_mortality, 0.5, 1440)
#
#       # Combine into a data frame
#       loop_results <- data.frame(
#         type = rep(c("typeI", "typeII", "typeIII"), each = n_mortality),
#         time = c(typeI_time, typeII_time, typeIII_time)
#       ) |>
#         dplyr::group_by(type) |>
#         dplyr::arrange(type, time) |>
#         dplyr::ungroup() |>
#         dplyr::mutate(n = rep(seq(1, n_mortality, 1), 3))
#       survivorship_loop[[i]] <- loop_results
#     }
#
#     survivorship_loop <- do.call(rbind, survivorship_loop)
#
#     survivorship_output_plot <- survivorship_output |>
#       dplyr::arrange(mortalitytime) |>
#       dplyr::mutate(id = seq(1, n_mortality, 1))
#
#     oldwarning <- getOption("warn")
#     options(warn = -1)
#     plot1 <- ggplot2::ggplot() +
#       ggplot2::theme_bw() + xlab("Time following larval release (hrs)") + ylab("Percent mortality") +
#       ggplot2::geom_point(data = survivorship_loop, ggplot2::aes(x = time/60, y = n, color = as.factor(type)), size = 1.5, alpha = 0.01) +
#       ggplot2::scale_color_manual(values = viridis::viridis(n = 4)[-4]) +
#       ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1.0, size = 2))) +
#       ggplot2::geom_point(data = survivorship_output_plot, ggplot2::aes(mortalitytime/60, as.numeric(id)), size = 2, shape = 21, show.legend = FALSE, fill = NA, color = "black") +
#       ggplot2::theme(legend.position = c(0.86,0.3), legend.title = element_blank(), legend.background = element_blank(),
#                      legend.key = element_blank(), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
#
#
#     return_list <- list(plot1, df_joined, survivorship_output)
#     names(return_list) <- c("simulated_mortality_plot", "simulated_mortality", "survivorship_output")
#     return(return_list)
#     options(warn=oldwarning)
#   } else if (return.plot == FALSE) {
#     return_list <- list(df_joined)
#     names(return_list) <- c("simulated_mortality")
#     return(return_list)
#
#   }
# }
