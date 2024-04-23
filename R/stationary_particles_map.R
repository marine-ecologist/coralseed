#' Map stationary particles
#'
#' Function to identify and map stationary particles in particle tracks
#'
#' @param input input particle tracks (e.g. coralseed:::Mermaid_PointSource_Bay_01)
#' @param subset n particles to subset
#' @export
#' stat_particles_liz22_del_14_33 <- stationary_particles(liz22_del_14_33)
#'
#' stat_liz22_del_14_33_map <- stationary_particles_map(liz22_del_14_33 |> filter(id %in% seq(0:1000)))
#' stat_liz22_del_14_33_map <- stationary_particles_map(coralseed:::Mermaid_PointSource_Bay_01 |> filter(id %in% seq(0:1000)))
#'
#' stat_liz22_del_14_33_map <- stationary_particles_map(run_day_12039_liz22_del_11_35|> filter(id %in% seq(0:100)))
#' stat_liz22_del_14_33_stationary <- stationary_particles(run_day_12039_liz22_del_11_35)

stationary_particles_map <- function(input, subset=100) {
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
      id_row=seq(1:n())) %>%
     select(id_row, is_stationary, id) %>%
    # pivot_wider(id_cols=id_row, names_from=id, values_from = is_stationary) |>
    mutate(id_row=as.numeric(id_row)*12) |>
    mutate(hours = id_row*60) |>
    as.data.frame()

  subset_output <- output |>
   # dplyr::mutate(id=as.numeric(id)) |>
    dplyr::filter(id %in% sample(seq(0:length(unique(output$id))-1),subset)) |>
    dplyr::mutate(id=as.factor(id))

  a <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::geom_tile(data=subset_output, ggplot2::aes(x=hours, y=id, fill=is_stationary)) +
    ggplot2::scale_fill_manual(values = c("lightblue", "firebrick3"))

  print(a)

  return(a)

}
