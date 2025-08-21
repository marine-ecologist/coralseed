#' Drop stuck particles
#'
#' Function to remove particles that remain at the same location for more than `n_time` consecutive timesteps.
#'
#' @param input input `sf` object containing particle tracks with time and id columns
#' @param n_time number of consecutive identical positions to consider as "stuck" (default: 100)
#' @param saveoutput optional file path to export the filtered dataset as GeoJSON
#' @export
#'

drop_stick <- function(input, n_time = 100, saveoutput = NULL) {

  tmp_particle_track <- input |>
    dplyr::mutate(dispersaltime = as.numeric(time - min(time)) / 60)

  has_long_repeats <- function(geom_vec) {
    geom_str <- as.character(geom_vec)
    runs <- rle(geom_str)
    any(runs$lengths > n_time)
  }

  if (!all(sf::st_dimension(tmp_particle_track) == 2)) {
    tmp_particle_track <- sf::st_zm(tmp_particle_track, drop = TRUE, what = "ZM")
  }

  ids_with_repeats <- tmp_particle_track |>
    dplyr::arrange(id, dispersaltime) |>
    dplyr::group_by(id) |>
    dplyr::summarise(long_repeats = has_long_repeats(geometry), .groups = "drop")

  ids_with_repeats_id <- ids_with_repeats |>
    dplyr::filter(long_repeats) |>
    dplyr::pull(id)

  cat(print(ids_with_repeats_id))

  output <- tmp_particle_track |>
    dplyr::filter(!id %in% ids_with_repeats_id) |>
    dplyr::arrange(id, dispersaltime) |>
    dplyr::filter(id %in% sample(unique(id), 10000))

  if (!is.null(saveoutput)) {
    sf::st_write(output, saveoutput, driver = "GeoJSON", quiet = TRUE)
  }

  return(output)
}



#
# drop_stick <- function(input, n_time = 100, saveoutput = NULL) {
#
#   tmp_particle_track <- input |>
#     dplyr::mutate(dispersaltime = as.numeric(time - min(time)) / 60)
#
#   has_long_repeats <- function(geom_vec) {
#     geom_str <- as.character(geom_vec)
#     runs <- rle(geom_str)
#     any(runs$lengths > n_time)
#   }
#
#   if (!all(sf::st_dimension(tmp_particle_track) == 2)) {
#     tmp_particle_track <- sf::st_zm(tmp_particle_track, drop = TRUE, what = "ZM")
#   }
#
#   ids_with_repeats <- tmp_particle_track |>
#     dplyr::arrange(id, dispersaltime) |>
#     dplyr::group_by(id) |>
#     dplyr::summarise(long_repeats = has_long_repeats(geometry), .groups = "drop")
#
#   ids_with_repeats_id <- ids_with_repeats |>
#     dplyr::filter(long_repeats) |>
#     dplyr::pull(id)
#
#   output <- tmp_particle_track |>
#     dplyr::filter(!id %in% ids_with_repeats_id) |>
#     dplyr::arrange(id, dispersaltime) |>
#     dplyr::filter(id %in% sample(unique(id), 10000))
#
#   if (!is.null(saveoutput)) {
#     sf::st_write(output, saveoutput, quiet = TRUE)
#   }
#
#   return(output)
# }
#
# # drop_stick <- function(input, n_time=100, saveoutput=NULL){
# #
# #   #tmp_particles <- st_read("/Users/rof011/Downloads/run_day_12036_lizard_del_14_1512_sim1_100K_10.json", quiet=TRUE)
# #
# #   tmp_particle_track <- input |>
# #     dplyr::mutate(dispersaltime = as.numeric(time-min(input$time))/60)
# #
# #   has_long_repeats <- function(geom_vec) {
# #     geom_str <- as.character(geom_vec)  # Convert POINT to string for rle
# #     runs <- rle(geom_str)
# #     any(runs$lengths > n_time)
# #   }
# #
# #   if (!all(st_dimension(tmp_particle_track) == 2)) {
# #     tmp_particle_track <- st_zm(tmp_particle_track, drop = TRUE, what = "ZM")
# #   }
# #
# #   ids_with_repeats <- tmp_particle_track |>
# #     dplyr::arrange(id, dispersaltime) |>
# #     dplyr::group_by(id) |>
# #     dplyr::summarise(long_repeats = has_long_repeats(geometry))
# #
# #   ids_with_repeats_id <- ids_with_repeats |>
# #     dplyr::filter(long_repeats) |>
# #     dplyr::pull(id)
# #
# #   output <- tmp_particle_track |>
# #     dplyr::filter(!id %in% ids_with_repeats_id)
# #     dplyr::arrange(id, dispersaltime) |>
# #     dplyr::filter(id %in% sample(unique(seed_particles_input$id), 10000))
# #
# #   if(~is.null(saveoutput)){
# #     sf::st_write(saveoutput, quiet=TRUE)
# #   }
# #   return(output)
# #
# # }
