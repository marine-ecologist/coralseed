#' Settle Particles
#'
#' Function to determine probability of settlement if particles pass suitable substrate
#' options: additive (if p=1, settle somewhere in that habitat along the particle trajectory regardless of time)
#'          lagged (if p=1, settle somewhere in the habitat in the first 10 minutes)
#'          rapid (if p=1, settle immediately habitat on trajectory )
#'
#'
#'
#' @param input input (defaults to particle_points_expanded from seed_particles() function)
#' @param probability one of "additive", "rapid", "lagged")
#' @param silent silence outputs
#' @param ... pass arguments
#' @export
#'

settle_particles <- function(input, probability = "additive", silent = TRUE, ...) {
  if (probability == "additive") {
    select_particles <- input |>
      dplyr::filter(outcome == "1") |> # dplyr::select settled particles
      dplyr::arrange(id, time) |>
      dplyr::group_by(id, habitat_id) |>
      dplyr::slice_sample(n = 1) |> # randomly sample a point in each habitat where competent
      dplyr::arrange(id, dispersaltime) |>
      dplyr::group_by(id) |>
      dplyr::slice_head(n = 1) |> # take first of the habitats by dispersaltime if multiple intersects
      dplyr::select(id, class, time, dispersaltime) |>
      dplyr::mutate(cat = "settled", cat = as.factor(cat))
  } else if (probability == "rapid") {
    select_particles <- input |>
      dplyr::filter(outcome == "1") |> # dplyr::select settled particles
      dplyr::arrange(id, time) |>
      dplyr::group_by(id, habitat_id) |>
      dplyr::slice_min(n = 1, order_by = dispersaltime) |> # take first value
      dplyr::arrange(id, dispersaltime) |>
      dplyr::group_by(id) |>
      dplyr::slice_head(n = 1) |> # take first of the habitats by dispersaltime if multiple intersects
      dplyr::select(id, class, time, dispersaltime) |>
      dplyr::mutate(cat = "settled", cat = as.factor(cat))
  } else if (probability == "lagged") { # (10 minute random dplyr::selection)
    select_particles <- input |>
      dplyr::filter(outcome == "1") |> # dplyr::select settled particles
      dplyr::arrange(id, time) |>
      dplyr::group_by(id, habitat_id) |>
      dplyr::slice_min(n = 10, order_by = dispersaltime) |> # take first ten values
      dplyr::slice_sample(n = 1) |> # take first five values
      dplyr::arrange(id, dispersaltime) |>
      dplyr::group_by(id) |>
      dplyr::slice_head(n = 1) |> # take first of the habitats by dispersaltime if multiple intersects
      dplyr::select(id, class, time, dispersaltime) |>
      dplyr::mutate(cat = "settled", cat = as.factor(cat))
  } else {
    print("probability is one of: additive / rapid / lagged")
  }

  ### for counter
  settled_particles_dispersaltime_df <- select_particles |>
    as.data.frame() |>
    dplyr::select(id, dispersaltime) |>
    dplyr::rename(maxdispersaltime = dispersaltime)
  if (silent == FALSE) {
    (cat(paste0("  \n")))
    (cat(paste0(nrow(settled_particles_dispersaltime_df), " / ", length(levels(as.factor(input$id))), " larvae settled \n")))
    (cat(paste0("  \n")))
  }
  ######

  settled_tracks <- input |>
    dplyr::filter(id %in% unique(settled_particles_dispersaltime_df$id)) |>
    dplyr::left_join(settled_particles_dispersaltime_df, by = "id") |>
    dplyr::filter(dispersaltime <= maxdispersaltime) |>
    dplyr::group_by(id) |>
    dplyr::summarise(do_union = FALSE) |>
    sf::st_cast("MULTILINESTRING") # |>

  results <- list(
    select_particles,
    settled_tracks
  )

  names(results) <- c("points", "paths")

  return(results)
}
