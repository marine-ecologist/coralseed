#' Map coralseed settlers
#'
#' Function to quickly map settlers
#'
#' @name extract_parallel
#' @param seed_particles_input input from seed_particles_input
#' @param settle_particles_input input from settle_particles_input
#' @param seascape_probability input from seascape_probability
#' @param restoration.plot dimensions of the restoration plot in metres
#' @param show.tracks option to show particle tracks (TRUE, will be large files and slower renders) or without tracks (FALSE, smaller files and faster renders)
#' @param scalebar set scale to X metres
#' @export
#'
#'


  # particle_rainbow <- seed_particles_input |>
  #   # remove duplicate geometries if particle is static or breaks linestring
  #   group_by(geometry) %>%
  #   slice_head(n = 1) %>%
  #   ungroup() %>%
  #   # make time bins
  #   dplyr::filter(dispersaltime %in% seq(0,1800,4)) %>%
  #   dplyr::mutate(dispersalbin = as.numeric(cut(dispersaltime, breaks = seq(0, 1800, 60), labels = seq(10, 1800, 60), include.lowest = TRUE))) %>%
  #   dplyr::arrange(id, dispersaltime) %>%
  #   dplyr::group_by(id, dispersalbin) %>%
  #   dplyr::summarise(do_union = FALSE) %>%
  #   sf::st_cast("MULTILINESTRING") %>%
  #   # drop intersections
  #   filter(st_is_valid(geometry))


map_coralseed <- function(seed_particles_input = particles, settle_particles_input = settlers,
                          seascape_probability = seascape, restoration.plot = c(100, 100),
                          show.tracks=TRUE, show.footprint=FALSE, subsample=NULL, scalebar=200, webGL=FALSE) {


  if (!is.null(subsample)) {
    seed_particles_input <- seed_particles_input |>
      dplyr::filter(id %in% sample(unique(particles$id), size = as.numeric(subsample)))

    settle_particles_input$paths <- settle_particles_input$paths |> dplyr::filter(id %in% unique(particles$id))
    settle_particles_input$points <- settle_particles_input$points |> dplyr::filter(id %in% unique(particles$id))
  }

  tracktimes <- 10  # take 5 minute time slices or paths become very long and complex
  particletracks <- seed_particles_input |>
    dplyr::filter(dispersaltime %in% seq(0,max(seed_particles_input$dispersaltime), tracktimes)) |>
    particles_to_tracks(by=c("id", "competency")) |>
    dplyr::select(-id)


  #particle_paths <- rbind(settle_particles_input$paths, settle_particles_input$paths[1:117,]) # why 1:117?
  particle_paths <- settle_particles_input$paths
  particle_points <- settle_particles_input$points |> mutate(dispersaltime2 = cut(dispersaltime, seq(1,max(particles$dispersaltime), 60), labels = FALSE))

  # kde2d heatmap
  particle_points_df <- particle_points |>
    sf::st_centroid() |>
    sf::st_buffer(1000) %>%
    sf::st_drop_geometry() %>%
    cbind(.,sf::st_coordinates(particle_points))

  particle_bbox <- particle_points |>
    sf::st_buffer(100) |>
    sf::st_bbox()

  kde_result <- MASS::kde2d(particle_points_df$X, particle_points_df$Y, n = 1000,
                            lims=c(particle_bbox[1], particle_bbox[3], particle_bbox[2], particle_bbox[4]))

  # Create a stars object from the kde2d result
  heatmap <- stars::st_as_stars(list(density = kde_result$z),
                                      dimensions = stars::st_dimensions(
                                        x = kde_result$x,
                                        y = kde_result$y
                                      )) |> sf::st_set_crs(st_crs(particle_points))

  # Set values below a threshold to NA
  max_value <- max(heatmap[[1]], na.rm = TRUE)
  threshold <- max_value / 20
  heatmap[[1]][heatmap[[1]] < threshold] <- NA

  # dims <- stars::st_dimensions(heatmap)
  # heatmap <- stars::st_as_stars(aperm(heatmap[[1]], c(2, 1))[nrow(heatmap[[1]]):1, , drop = FALSE],
  #                              dimensions = dims)

  ## particle_rainbow for all particles (computationally taxing so only show settled? Or subset
  # particle_rainbow <- seed_particles_input |>
  # #   # remove duplicate geometries if particle is static or breaks linestring
  #    dplyr::group_by(geometry) %>%
  #    tidyr::slice_head(n = 1) %>%
  #    dplyr::ungroup() %>%
  #    # make time bins
  #    dplyr::filter(dispersaltime %in% seq(0,1800,5)) %>%
  #    dplyr::mutate(dispersalbin = as.numeric(as.character((cut(dispersaltime, breaks = seq(0, 1800, 60), labels = seq(1, 1800, 60), include.lowest = TRUE))))-1) %>%
  #    dplyr::arrange(id, dispersaltime) %>%
  #    dplyr::group_by(id, dispersalbin) %>%
  #    dplyr::summarise(do_union = FALSE) %>%
  #    sf::st_cast("MULTILINESTRING") %>%
  # #   # drop intersections
  #    filter(st_is_valid(geometry))

  maxdispersal <-  ceiling(max(seed_particles_input$dispersaltime) / 60) * 60

  particle_rainbow_points <- seed_particles_input %>%
    dplyr::filter(dispersaltime %in% seq(0, maxdispersal, 10)) %>%
    mutate(dispersalbin = cut(dispersaltime, breaks = seq(0, maxdispersal, 60))) %>%
# note: revert to labels for tm_continuous fixed, this is a workaround for tm_ordinal
#   dplyr::mutate(dispersalbin = as.numeric(as.character(cut(dispersaltime, breaks = seq(0, max(seed_particles_input$dispersaltime), 60), labels = seq(1, max(seed_particles_input$dispersaltime), 60), include.lowest = TRUE)))) %>%
    dplyr::arrange(id) %>% # Ensure data is sorted by id for lag function
    dplyr::group_by(id, competency) %>%
    dplyr::mutate(
      geometry_lagged = dplyr::lag(geometry, default =  sf::st_as_sfc("POINT(EMPTY)", crs = 20353))
    ) %>%
    dplyr::slice(-1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      line = sf::st_sfc(purrr::map2(
        .x = geometry,
        .y = geometry_lagged,
        .f = ~{sf::st_union(c(.x, .y)) %>% sf::st_cast("LINESTRING")}
      ))) %>%
    dplyr::select(id, line, dispersalbin)

particle_rainbow <- particle_rainbow_points |>
    sf::st_sf(geometry = sf::st_sfc(particle_rainbow_points$line, crs = sf::st_crs(particle_rainbow_points))) |>
    dplyr::arrange(id, dispersalbin) |> dplyr::select(-id)

  if (webGL==TRUE){
    tmap::tm_view(use.webGL=TRUE)
  } else {
    tmap::tm_view()
  }

  tmap_mode("view")

  tmp <-

    # # seascape habitats
    # tmap::tm_shape(seascape_probability, name = "<b> [Seascape]</b> habitats") +
    # tmap::tm_polygons(fill="class",
    #                   lwd = 0.2, lwd.color="black",
    #                   fill.alpha = 0.6,
    #                   fill.legend = tmap::tm_legend(title = "Benthic habitats"),
    #                   fill.scale = tmap::tm_scale_categorical(values = seascapecolors())) +
    #
    # # seascape habitats
    # tmap::tm_shape(seascape_probability, name = "<b> [Seascape]</b> probability") +
    # tmap::tm_polygons(fill="settlement_probability",
    #                   lwd = 0.2, lwd.color="black",
    #                   fill.alpha = 0.6,
    #                   fill.legend = tmap::tm_legend(title = "Settlement probability"),
    #                   fill.scale = tmap::tm_scale_continuous(values="reds")) +

#
#     # particle tracks competency
#     tmap::tm_shape(particletracks, name = "<b> [Particles]</b> competency") +
#     tmap::tm_lines("competency",
#                    lwd = 0.8,
#                    fill.legend = tmap::tm_legend(title = "Competency"),
#                    tmap::tm_scale_categorical(values= c("lightblue", "cadetblue4"))) +
#
#     # particle tracks
#     tmap::tm_shape(particletracks, name =  "<b> [Particles]</b> dispersal tracks") +
#     tmap::tm_lines(col="grey",
#                    lwd = 0.8,
#                    fill.legend = tmap::tm_legend(title = "Dispersal tracks")) +
#
#     # pre-settlement tracks
#     tmap::tm_shape(particle_paths, name =  "<b> [Settlers]</b> Dispersal tracks") +
#     tmap::tm_lines(col="grey",
#                    lwd = 0.8,
#                    fill.legend = tmap::tm_legend(title = "Settled larval tracks")) +
#
#     # pre-settlement tracks
#     tmap::tm_shape(particle_paths, name =  "<b> [Settlers]</b> pre-settlement tracks") +
#     tmap::tm_lines(col="grey",
#                    lwd = 0.8,
#                    fill.legend = tmap::tm_legend(title = "Settled larval tracks")) +
#
    # kde2d heatmap
    tmap::tm_shape(heatmap) +#, name = "<b> [Stats]</b> post-settlement heatmap") +
    tmap::tm_raster(col.scale = tmap::tm_scale_continuous(values = "gn_bu"),
                    col.legend = tmap::tm_legend_hide(),
                    col.alpha = tmap::tm_const()) +

    #particle points
    tmap::tm_shape(particle_points, is.main=TRUE, id="dispersaltime2", name = "<b> [Settlers]</b> post-settlement location") +
    tmap::tm_dots(fill="dispersaltime2", shape=21, lwd.color="black",
                  fill.scale = tmap::tm_scale_continuous(values="-spectral"),
                  fill.legend = tmap::tm_legend("Dispersal time \n(hours)")) +
#
#     # settlement grid
#     tmap::tm_shape(settler_density$density, name = "<b> [Stats]</b> spatial grid") +
#     tmap::tm_borders(lwd = 0.5,
#                      col = "black") +
#
#     # settlement count
#     tmap::tm_shape(settler_density$count, name = "<b> [Stats]</b> settlement count") +
#     tmap::tm_polygons(fill="count",
#                       lwd = 0,
#                       fill.alpha = 0.6,
#                       fill.scale = tmap::tm_scale_continuous(values="-plasma", value.na = "transparent"),
#                       fill.legend = tmap::tm_legend("Settlement count")) +
#
#     # settlement density
#     tmap::tm_shape(settler_density$density, name = "<b> [Stats]</b> settlement density") +
#     tmap::tm_polygons(fill="density",
#                       lwd=0,
#                       fill.alpha = 0.6,
#                       fill.scale = tmap::tm_scale_continuous(values="-magma", value.na = "transparent"),
#                       fill.legend = tmap::tm_legend("Settlement density")) +
#
#     # restoration plot
#     tmap::tm_shape(restoration_plot, name = "<b> [Stats]</b> restoration hectare") +
#     tmap::tm_borders(lwd = 2,
#                      col = "red") +

    # tmap options
    tmap::tmap_options(check.and.fix = TRUE,
                       show.messages=FALSE,
                       show.warnings=FALSE) +

    tmap::tm_credits("https://github.com/marine-ecologist/coralseed/")

  if(show.tracks==TRUE){

  tmp <- tmp +

    # rainbow tracks - note tm_ordinal, change to continuous when tmap fixed
    tmap::tm_shape(particle_rainbow,
                   name = "<b> [Particles]</b> dispersaltime") +
    tmap::tm_lines(col="dispersalbin",
                   lwd = 0.8,
                   tmap::tm_scale_ordinal("-spectral", n.max = 12),
                   col.legend = tmap::tm_legend(title = "Dispersal time"))

  }

  if (!is.null(scalebar)){
  tmp <-  tmp +
    tmap::tm_scalebar(width = scalebar)
        }

  if (show.footprint==TRUE){
  tmp <- tmp +
  # post-settlement area
  tmap::tm_shape(settler_density$area, id="area", name = "<b> [Settlers]</b> post-settlement area") +
    tmap::tm_polygons(values="polygons", lwd = 0.8,
                      fill="indianred4",
                      fill.alpha=0.6,
                      fill.legend = tmap::tm_legend(title = "Restoration footprint"))

  }

  tmp
  # tmp |> tmap::tmap_leaflet() |>
  #   leaflet::addProviderTiles('Esri.WorldImagery', group = "<b> [Seascape]</b> satellite map", options=leaflet::providerTileOptions(maxNativeZoom=18,maxZoom=100)) |>
  #   leaflet::addProviderTiles('Esri.WorldTopoMap',  group = "<b> [Seascape]</b> base map", options=leaflet::providerTileOptions(maxNativeZoom=18,maxZoom=100)) |>
  #   leaflet::addLayersControl(position="topleft", overlayGroups=c("<b> [Seascape]</b> satellite map",
  #                                                                 "<b> [Seascape]</b> habitats",
  #                                                                 "<b> [Seascape]</b> probability",
  #                                                                 "<b> [Particles]</b> dispersal tracks",
  #                                                                 "<b> [Particles]</b> dispersaltime",
  #                                                                 "<b> [Particles]</b> competency",
  #                                                                 "<b> [Settlers]</b> pre-settlement tracks",
  #                                                                 "<b> [Stats]</b> post-settlement heatmap",
  #                                                                 "<b> [Settlers]</b> post-settlement area",
  #                                                                 "<b> [Stats]</b> spatial grid",
  #                                                                 "<b> [Stats]</b> settlement count",
  #                                                                 "<b> [Stats]</b> settlement density",
  #                                                                 "<b> [Stats]</b> restoration hectare"),
  #                             options=leaflet::layersControlOptions(collapsed = FALSE)) |>
  #   leaflet::hideGroup(c("<b> [Seascape]</b> base map",
  #                        "<b> [Seascape]</b> probability",
  #                        "<b> [Particles]</b> dispersaltime",
  #                        "<b> [Particles]</b> competency",
  #                        "<b> [Particles]</b> dispersal tracks",
  #                        "<b> [Settlers]</b> post-settlement area",
  #                        "<b> [Stats]</b> spatial grid",
  #                        "<b> [Stats]</b> settlement count",
  #                        "<b> [Stats]</b> settlement density")) |>
  #   leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)



}
