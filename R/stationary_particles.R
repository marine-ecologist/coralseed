#' Stationary particles
#'
#' Function to identify stationary particles in particle tracks
#'
#' @param input input particle tracks (e.g. coralseed:::Mermaid_PointSource_Bay_01)
#' @param subset n particles to subset
#'
#' stat_particles_liz22_del_14_33 <- stationary_particles(liz22_del_14_33)
#'
#' stat_liz22_del_14_33_map <- stationary_particles_map(liz22_del_14_33 |> filter(id %in% seq(0:1000)))
#' stat_liz22_del_14_33_map <- stationary_particles_map(coralseed:::Mermaid_PointSource_Bay_01 |> filter(id %in% seq(0:1000)))
#'
#' stat_liz22_del_14_33_map <- stationary_particles_map(run_day_12039_liz22_del_11_35|> filter(id %in% seq(0:100)))
#' stat_liz22_del_14_33_stationary <- stationary_particles(run_day_12039_liz22_del_11_35)
#'
#'
stationary_particles <- function(input) {
    start <- Sys.time ()
    output <- input %>%
      dplyr::arrange(id, time) %>%
      dplyr::mutate(
        geometry_lag = dplyr::lag(geometry),
        time_lag = dplyr::lag(time),
        time_diff = as.numeric(difftime(time, time_lag, units = "mins")), # Calculate time difference in minutes
        is_stationary = geometry == geometry_lag
      ) %>%
      as.data.frame() %>%
      dplyr::group_by(id) %>%
      # Calculate both stationary and moving times
      dplyr::mutate(
        # Set the first row's time_diff to NA for each group
        time_diff = if_else(row_number() == 1, NA_real_, time_diff),
        is_stationary = if_else(row_number() == 1, FALSE, is_stationary) # Optionally handle is_stationary similarly
      ) %>%
      dplyr::summarize(
        total_stationary_time = sum(time_diff[is_stationary], na.rm = TRUE),
        total_moving_time = sum(time_diff[!is_stationary], na.rm = TRUE)
      ) %>%
      dplyr::mutate(sum_time=total_stationary_time+total_moving_time) %>%
      dplyr::mutate(proportion_stationary=total_stationary_time/sum_time*100) %>%
      dplyr::ungroup()



      a <- ggplot2::ggplot(output, ggplot2::aes(x = proportion_stationary)) +
        ggplot2::theme_bw() + ggplot2::xlim(-10,110) +  ggplot2::ylim(0,100) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count/sum(count)) * 100),
                                fill="lightblue", color="black", binwidth = 5) +
        ggplot2::ylab("Proportion of particles (%)") +
        ggplot2::xlab("Mean proportion of time stationary (%)")

      print(a)

      print(round(Sys.time () - start),3)
    return(output)

}
