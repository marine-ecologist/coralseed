#' Settle Particles
#'
#' Function to determine probability of settlement if particles pass suitable substrate.
#' Options:
#' - "additive": settle somewhere in habitat intersect regardless of time
#' - "lagged": sample first 10 minutes of habitat entry
#' - "rapid": settle at first habitat intersect
#'
#' @param input output from `seed_particles()` or directly a particle `sf` object
#' @param seascape sf object from `seascape_probability()`, for plotting base
#' @param probability settlement rule, one of "additive", "rapid", or "lagged"
#' @param silent suppress printed output (default TRUE)
#' @param return.plot whether to return tmap plot output
#' @param subsample optionally sample N particles for faster plotting
#' @param seed.value set seed for reproducibility
#' @param ... additional args
#' @export
#'

settle_particles <- function(input, seascape = seascape, probability = "additive",
                             silent = TRUE, return.plot = FALSE,
                             subsample = NULL, seed.value = NULL, ...) {

  if (is.list(input)) {
    input <- input$seed_particles
  }

  set.seed(seed.value)

  if (probability == "additive") {
    select_particles <- input |>
      dplyr::filter(outcome == "1") |>
      dplyr::arrange(id, time) |>
      dplyr::group_by(id, habitat_id) |>
      dplyr::slice_sample(n = 1) |>
      dplyr::arrange(id, dispersaltime) |>
      dplyr::group_by(id) |>
      dplyr::slice_head(n = 1) |>
      dplyr::select(id, class, time, dispersaltime) |>
      dplyr::mutate(cat = as.factor("settled"))

  } else if (probability == "rapid") {
    select_particles <- input |>
      dplyr::filter(outcome == "1") |>
      dplyr::arrange(id, time) |>
      dplyr::group_by(id, habitat_id) |>
      dplyr::slice_min(n = 1, order_by = dispersaltime) |>
      dplyr::arrange(id, dispersaltime) |>
      dplyr::group_by(id) |>
      dplyr::slice_head(n = 1) |>
      dplyr::select(id, class, time, dispersaltime) |>
      dplyr::mutate(cat = as.factor("settled"))

  } else if (probability == "lagged") {
    select_particles <- input |>
      dplyr::filter(outcome == "1") |>
      dplyr::arrange(id, time) |>
      dplyr::group_by(id, habitat_id) |>
      dplyr::slice_min(n = 10, order_by = dispersaltime) |>
      dplyr::slice_sample(n = 1) |>
      dplyr::arrange(id, dispersaltime) |>
      dplyr::group_by(id) |>
      dplyr::slice_head(n = 1) |>
      dplyr::select(id, class, time, dispersaltime) |>
      dplyr::mutate(cat = as.factor("settled"))

  } else {
    stop("probability must be one of: 'additive', 'rapid', 'lagged'")
  }

  if (!is.null(subsample)) {
    select_particles <- select_particles |>
      dplyr::ungroup() |>
      dplyr::distinct(id) |>
      dplyr::slice_sample(n = subsample) |>
      dplyr::semi_join(select_particles, by = "id")
  }

  settled_particles_dispersaltime_df <- select_particles |>
    dplyr::select(id, dispersaltime) |>
    dplyr::rename(maxdispersaltime = dispersaltime) |>
    as.data.frame()

  if (!silent) {
    cat("\n")
    cat(paste0(nrow(settled_particles_dispersaltime_df), " / ", length(unique(input$id)), " larvae settled\n\n"))
  }

  settled_tracks <- input |>
    dplyr::filter(id %in% settled_particles_dispersaltime_df$id) |>
    dplyr::left_join(settled_particles_dispersaltime_df, by = "id") |>
    dplyr::filter(dispersaltime <= maxdispersaltime) |>
    dplyr::group_by(id) |>
    dplyr::summarise(do_union = FALSE, dispersaltime = mean(dispersaltime), .groups = "drop") |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_make_valid() %>%
    dplyr::mutate(distance = sf::st_length(.))

  results <- list(
    points = select_particles,
    paths = settled_tracks
  )

  if (return.plot) {
    tmap::tmap_mode("plot")
    settlemap <- tm_basemap("Esri.WorldImagery", alpha = 0.8) +
      tmap::tm_shape(seascape, bbox = sf::st_bbox(results$paths |> sf::st_buffer(50))) +
      tmap::tm_polygons("class", col = "black", lwd = 0.2, alpha = 0.2,
                        legend.show = TRUE,
                        palette = seascape_color_pal()) +
      tmap::tm_shape(results$paths) +
      tmap::tm_lines(col = "white", lwd = 0.2) +
      tmap::tm_shape(results$points) +
      tmap::tm_dots(col = "dispersaltime", shape = 21, size = 0.25,
                    palette = "-Spectral") +
      tmap::tm_options(component.autoscale = FALSE)

    print(settlemap)
  }

  return(results)
}

# settle_particles <- function(input, probability = "additive", silent = TRUE, return.plot=FALSE, subsample=NULL, seed.value=NULL,...) {
#
#    if (is.list(input)) {
#     input <- input$seed_particles
#   } else {
#     input = input
#     }
#
#     set.seed(seed.value)
#
#   if (probability == "additive") {
#     select_particles <- input |>
#       dplyr::filter(outcome == "1") |> # dplyr::select settled particles
#       dplyr::arrange(id, time) |>
#       dplyr::group_by(id, habitat_id) |>
#       dplyr::slice_sample(n = 1) |> # randomly sample a point in each habitat where competent
#       dplyr::arrange(id, dispersaltime) |>
#       dplyr::group_by(id) |>
#       dplyr::slice_head(n = 1) |> # take first of the habitats by dispersaltime if multiple intersects
#       dplyr::select(id, class, time, dispersaltime) |>
#       dplyr::mutate(cat = "settled", cat = as.factor(cat))
#   } else if (probability == "rapid") {
#     select_particles <- input |>
#       dplyr::filter(outcome == "1") |> # dplyr::select settled particles
#       dplyr::arrange(id, time) |>
#       dplyr::group_by(id, habitat_id) |>
#       dplyr::slice_min(n = 1, order_by = dispersaltime) |> # take first value
#       dplyr::arrange(id, dispersaltime) |>
#       dplyr::group_by(id) |>
#       dplyr::slice_head(n = 1) |> # take first of the habitats by dispersaltime if multiple intersects
#       dplyr::select(id, class, time, dispersaltime) |>
#       dplyr::mutate(cat = "settled", cat = as.factor(cat))
#   } else if (probability == "lagged") { # (10 minute random dplyr::selection)
#     select_particles <- input |>
#       dplyr::filter(outcome == "1") |> # dplyr::select settled particles
#       dplyr::arrange(id, time) |>
#       dplyr::group_by(id, habitat_id) |>
#       dplyr::slice_min(n = 10, order_by = dispersaltime) |> # take first ten values
#       dplyr::slice_sample(n = 1) |> # take first five values
#       dplyr::arrange(id, dispersaltime) |>
#       dplyr::group_by(id) |>
#       dplyr::slice_head(n = 1) |> # take first of the habitats by dispersaltime if multiple intersects
#       dplyr::select(id, class, time, dispersaltime) |>
#       dplyr::mutate(cat = "settled", cat = as.factor(cat))
#   } else {
#     print("probability is one of: additive / rapid / lagged")
#   }
#
#   if (!is.null(subsample)) {
#      select_particles_isolate <- select_particles %>%
#       dplyr::ungroup() %>%
#       dplyr::distinct(id) %>%
#       dplyr::slice_sample(n = subsample)
#
#      select_particles <- select_particles |> dplyr::filter(id %in% select_particles_isolate$id)
#   }
#
#   ### for counter
#   settled_particles_dispersaltime_df <- select_particles |>
#     as.data.frame() |>
#     dplyr::select(id, dispersaltime) |>
#     dplyr::rename(maxdispersaltime = dispersaltime)
#   if (silent == FALSE) {
#     (cat(paste0("  \n")))
#     (cat(paste0(nrow(settled_particles_dispersaltime_df), " / ", length(levels(as.factor(input$id))), " larvae settled \n")))
#     (cat(paste0("  \n")))
#   }
#   ######
#   # if (return.plot == TRUE) {
#   #
#   #   settled_particles_dispersaltime_df_plot <- settled_particles_dispersaltime_df |>
#   #     mutate(y=rev(seq(1:n())))
#   #
#   #
#   #   timetosettlement <- ggplot() + theme_bw() + xlab("Time to settlement") +
#   #     geom_point(data=settled_particles_dispersaltime_df_plot, aes(x=maxdispersaltime, y=y), size=0.5)
#   #
#   #   print(timetosettlement)
#   #
#   # }
#
#
#   settled_tracks <- input |>
#     dplyr::filter(id %in% unique(settled_particles_dispersaltime_df$id)) |>
#     dplyr::left_join(settled_particles_dispersaltime_df, by = "id") |>
#     dplyr::filter(dispersaltime <= maxdispersaltime) |>
#     dplyr::group_by(id) |>
#     dplyr::summarise(do_union = FALSE, dispersaltime=mean(dispersaltime)) |>
#     sf::st_cast("MULTILINESTRING") |>
#     sf::st_make_valid() %>%
#     dplyr::mutate(distance=sf::st_length(.))
#
#   results <- list(
#     select_particles,
#     settled_tracks
#   )
#
#   names(results) <- c("points", "paths")
#
#   #set.seed(NULL)
#   return(results)
# }
