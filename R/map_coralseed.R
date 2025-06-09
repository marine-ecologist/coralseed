#' Map coralseed settlers
#'
#' Function to quickly map settlers
#'
#' @name extract_parallel
#' @param seed_particles_input input from seed_particles_input
#' @param settle_particles_input input from settle_particles_input
#' @param settlement_density_input input from settlement density
#' @param seascape_probability input from seascape_probability
#' @param restoration.plot dimensions of the restoration plot in metres
#' @param show.tracks option to show particle tracks (TRUE, will be large files and slower renders)
#' @param show.footprint show spatial footprint (TRUE/FALSE)
#' @param scalebar set scale to X metres
#' @param subsample subsample large datasets for visualisation (n)
#' @param heatmap_res spatial resolution of heatmap
#' @param heatmap_buffer spatial buffer around heatmap in metres
#' @param webGL use webGL in tmap? TRUE/FALSE
#' @export
#'

map_coralseed <- function(seed_particles_input = NULL, settle_particles_input = NULL,
                          settlement_density_input = NULL, crs = 20353,
                          seascape_probability = NULL, restoration.plot = c(100, 100),
                          show.tracks = TRUE, show.footprint = FALSE, subsample = NULL,
                          heatmap_res = 2, heatmap_buffer = 0.25, scalebar = 200, webGL = FALSE) {

  if (is.list(seed_particles_input)) {
    seed_particles_input <- seed_particles_input$seed_particles
  }

  if(!st_crs(seed_particles_input) == crs){
    seed_particles_input <-  st_transform(seed_particles_input, crs)
  }


  if (!is.null(subsample)) {
    seed_particles_input <- seed_particles_input |>
      dplyr::filter(id %in% sample(unique(seed_particles_input$id), size = as.numeric(subsample)))

    settle_particles_input$paths <- settle_particles_input$paths |> dplyr::filter(id %in% unique(seed_particles_input$id))
    settle_particles_input$points <- settle_particles_input$points
  }

  tracktimes <- 10
  particletracks <- seed_particles_input |>
    dplyr::filter(dispersaltime %in% seq(0, max(seed_particles_input$dispersaltime), tracktimes)) |>
    particles_to_tracks(by = c("id", "competency")) |>
    dplyr::select(-id)

  particle_paths <- settle_particles_input$paths
  particle_points <- settle_particles_input$points |>
    dplyr::mutate(dispersaltime2 = cut(dispersaltime, seq(1, max(settle_particles_input$points$dispersaltime, na.rm = TRUE), 60), labels = FALSE))

    if(!st_crs(particle_paths) == crs){
      particle_paths <-  st_transform(particle_paths, crs)
    }

    if(!st_crs(particle_points) == crs){
      particle_points <-  st_transform(particle_points, crs)
    }


  heatmap_coralseed <- settler_heatmap(settle_particles_input$points,
                                       xres = heatmap_res,
                                       yres = heatmap_res,
                                       buffer_factor = heatmap_buffer) |>
    terra::rast()

  maxdispersal <- ceiling(max(seed_particles_input$dispersaltime) / 60) * 60



  particle_rainbow_points <- seed_particles_input |>
    dplyr::filter(dispersaltime %in% seq(0, maxdispersal, 10)) |>
    dplyr::arrange(id) |>
    dplyr::group_by(id, competency) |>
    dplyr::mutate(geometry_lagged = dplyr::lag(geometry, default = NA)) |>
    dplyr::slice(-1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      geometry_lagged = ifelse(is.na(geometry_lagged), geometry, geometry_lagged),
      line = sf::st_sfc(purrr::map2(geometry, geometry_lagged, ~ sf::st_cast(sf::st_union(c(.x, .y)), "LINESTRING")))
    ) |>
    dplyr::select(id, line, dispersaltime)

  particle_rainbow <- particle_rainbow_points |>
    dplyr::select(id, dispersaltime, line) |>
    sf::st_as_sf(sf_column_name = "line") |>
    dplyr::arrange(id, dispersaltime) |>
    dplyr::select(-id)

  # particle_rainbow <- sf::st_sf(geometry = sf::st_sfc(particle_rainbow_points$line,
  #                                                     crs = sf::st_crs(particle_rainbow_points))) |>
  #   dplyr::arrange(id, dispersaltime) |>
  #   dplyr::select(-id)

  seascape_color_pal <- c(
    "Plateau" = "cornsilk2",
    "Back Reef Slope" = "darkcyan",
    "Reef Slope" = "darkseagreen4",
    "Sheltered Reef Slope" = "darkslategrey",
    "Inner Reef Flat" = "darkgoldenrod4",
    "Outer Reef Flat" = "darkgoldenrod2",
    "Reef Crest" = "coral3"
  ) %>% rlang::set_names(names(.))

  if(!st_crs(seascape_probability) == crs){
    seascape_probability <-  st_transform(seascape_probability, crs)
  }

  restoration_plot <- seed_particles_input |>
    dplyr::filter(dispersaltime == 0) |>
    dplyr::slice(1) |>
    sf::st_centroid() |>
    coralseed::set_restoration_plot(restoration.plot[1], restoration.plot[2],
                                    crs = sf::st_crs(seed_particles_input))

  tmap::tmap_mode("view")
  if (webGL) tmap::tm_view(use_WebGL = TRUE)

  tmp <- tm_basemap("Esri.WorldImagery") +

    tmap::tm_shape(seascape_probability, name = "<b> [Seascape]</b> habitats") +
    tmap::tm_polygons(fill = "class", lwd = 0.2, col = "black", fill_alpha = 0.6,
                      fill.legend = tmap::tm_legend("Benthic habitats"),
                      fill.scale = tmap::tm_scale_categorical(values = seascape_color_pal)) +

    tmap::tm_shape(seascape_probability, name = "<b> [Seascape]</b> probability") +
    tmap::tm_polygons(fill = "settlement_probability", lwd = 0.2, col = "black", fill_alpha = 0.6,
                      lty.legend = tmap::tm_legend("Settlement probability"),
                      fill.scale = tmap::tm_scale_continuous(values = "brewer.reds")) +

    tmap::tm_shape(particletracks, name = "<b> [Particles]</b> competency") +
    tmap::tm_lines("competency", lwd = 0.8,
                   lty.legend = tmap::tm_legend("Competency"),
                   tmap::tm_scale_categorical(values = c("lightblue", "cadetblue4"))) +

    tmap::tm_shape(particle_paths, name = "<b> [Settlers]</b> pre-settlement tracks") +
    tmap::tm_lines(col = "grey", lwd = 0.8,
                   lty.legend = tmap::tm_legend("Settled larval tracks")) +

    tmap::tm_shape(heatmap_coralseed, name = "<b> [Stats]</b> settlement heatmap") +
    tmap::tm_raster(col.scale = tmap::tm_scale_continuous(values = "gn_bu"),
                    col.legend = tmap::tm_legend_hide(),
                    col_alpha = tmap::tm_const()) +

    tmap::tm_shape(particle_points, is.main = TRUE, id = "dispersaltime2", name = "<b> [Settlers]</b> post-settlement location") +
    tmap::tm_dots(fill = "dispersaltime2", shape = 21, col = "black",
                  fill.scale = tmap::tm_scale_continuous(values = "-brewer.spectral"),
                  fill.legend = tmap::tm_legend("Dispersal time \n(hours)")) +

    tmap::tm_shape(settlement_density_input$area, name = "<b> [Settlers]</b> restoration footprint") +
    tmap::tm_dots(fill = "coral", fill_alpha = 0.3) +

    tmap::tm_shape(settlement_density_input$density, name = "<b> [Stats]</b> spatial grid") +
    tmap::tm_borders(lwd = 0.15, col = "black") +

    tmap::tm_shape(settlement_density_input$count, name = "<b> [Stats]</b> settlement count") +
    tmap::tm_polygons(fill = "count", lwd = 0, fill_alpha = 0.6,
                      fill.scale = tmap::tm_scale_continuous(values = "-plasma"),
                      fill.legend = tmap::tm_legend("Settlement count")) +

    tmap::tm_shape(settlement_density_input$density, name = "<b> [Stats]</b> settlement density") +
    tmap::tm_polygons(fill = "density", lwd = 0, fill_alpha = 0.6,
                      fill.scale = tmap::tm_scale_continuous(values = "-magma"),
                      fill.legend = tmap::tm_legend("Settlement density")) +

    tmap::tm_shape(restoration_plot, name = "<b> [Stats]</b> restoration hectare") +
    tmap::tm_borders(lwd = 2, col = "red") +

    tmap::tmap_options(show.messages = FALSE, show.warnings = FALSE) +

    tmap::tm_credits("https://github.com/marine-ecologist/coralseed/")

  if (show.tracks) {
    tmp <- tmp +
      tmap::tm_shape(particle_rainbow, name = "<b> [Particles]</b> dispersaltime") +
      tmap::tm_lines(col = "dispersaltime", lwd = 0.8,
                     tmap::tm_scale_continuous(values = "-brewer.spectral"),
                     col.legend = tmap::tm_legend("Dispersal time"))
  }

  if (!is.null(scalebar)) {
    tmp <- tmp + tmap::tm_scalebar(width = scalebar)
  }

  if (show.footprint) {
    tmp <- tmp +
      tmap::tm_shape(settlement_density_input$area, id = "area", name = "<b> [Settlers]</b> post-settlement area") +
      tmap::tm_polygons(lwd = 0.8, fill = "indianred4", fill_alpha = 0.6,
                        fill.legend = tmap::tm_legend("Restoration footprint"))
  }

  tmp |> tmap::tmap_leaflet() |>
    leaflet::addProviderTiles('Esri.WorldImagery', group = "<b> [Seascape]</b> satellite map") |>
    leaflet::addProviderTiles('Esri.WorldTopoMap', group = "<b> [Seascape]</b> base map") |>
    leaflet::addLayersControl(position = "topleft",
                              overlayGroups = c(
                                "<b> [Seascape]</b> satellite map",
                                "<b> [Seascape]</b> habitats",
                                "<b> [Seascape]</b> probability",
                                "<b> [Particles]</b> dispersal tracks",
                                "<b> [Particles]</b> dispersaltime",
                                "<b> [Particles]</b> competency",
                                "<b> [Settlers]</b> pre-settlement tracks",
                                "<b> [Settlers]</b> post-settlement area",
                                "<b> [Settlers]</b> post-settlement location",
                                "<b> [Stats]</b> spatial grid",
                                "<b> [Stats]</b> settlement count",
                                "<b> [Stats]</b> settlement density",
                                "<b> [Stats]</b> settlement heatmap",
                                "<b> [Stats]</b> restoration hectare",
                                "<b> [Settlers]</b> restoration footprint"
                              ),
                              options = leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::hideGroup(c(
      "<b> [Seascape]</b> base map",
      "<b> [Seascape]</b> probability",
      "<b> [Particles]</b> dispersaltime",
      "<b> [Particles]</b> competency",
      "<b> [Particles]</b> dispersal tracks",
      "<b> [Settlers]</b> post-settlement area",
      "<b> [Stats]</b> spatial grid",
      "<b> [Stats]</b> settlement count",
      "<b> [Stats]</b> settlement heatmap",
      "<b> [Stats]</b> settlement density",
      "<b> [Settlers]</b> restoration footprint"
    )) |>
    leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)
}


#
# map_coralseed <- function(seed_particles_input = NULL, settle_particles_input = NULL,
#                           settlement_density_input = NULL,
#                           seascape_probability = NULL, restoration.plot = c(100, 100),
#                           show.tracks=TRUE, show.footprint=FALSE, subsample = NULL,
#                           heatmap_res=2, heatmap_buffer=0.25, scalebar=200, webGL=FALSE) {
#
#
#   if (is.list(seed_particles_input)) {
#     seed_particles_input <- seed_particles_input$seed_particles
#   }
#
#
#   if (!is.null(subsample)) {
#     seed_particles_input <- seed_particles_input |>
#       dplyr::filter(id %in% sample(unique(seed_particles_input$id), size = as.numeric(subsample)))
#
#     settle_particles_input$paths <- settle_particles_input$paths |> dplyr::filter(id %in% unique(seed_particles_input$id))
#     settle_particles_input$points <- settle_particles_input$points #|> dplyr::filter(id %in% unique(seed_particles_input$id))
#   }
#
#   tracktimes <- 10  # take 5/10 minute time slices or paths become very long and complex
#   particletracks <- seed_particles_input |>
#     dplyr::filter(dispersaltime %in% seq(0,max(seed_particles_input$dispersaltime), tracktimes)) |>
#     particles_to_tracks(by=c("id", "competency")) |>
#     dplyr::select(-id)
#
#   #cat("1 \n")
#
#   particle_paths <- settle_particles_input$paths
#   particle_points <- settle_particles_input$points |> dplyr::mutate(dispersaltime2 = cut(dispersaltime, seq(1,max(settle_particles_input$points$dispersaltime, na.rm=TRUE), 60), labels = FALSE))
#
#
#   heatmap_coralseed <- settler_heatmap(settle_particles_input$points, xres = heatmap_res, yres = heatmap_res, buffer_factor = heatmap_buffer) |> terra::rast()
#
#   #cat("2 \n")
#   # tmap::tm_shape(heatmap_coralseed, name = "<b> [Stats]</b> settlement heatmap") +
#   #   tmap::tm_raster()
#   #
#   #     col.scale = tmap::tm_scale_continuous(values = "gn_bu"),
#   #     col.legend = tmap::tm_legend_hide(),
#   #     col_alpha = tmap::tm_const()
#   #   )
#
#   ## particle_rainbow for all particles (computationally taxing so only show settled? Or subset
#   # particle_rainbow <- seed_particles_input |>
#   # #   # remove duplicate geometries if particle is static or breaks linestring
#   #    dplyr::group_by(geometry) %>%
#   #    tidyr::slice_head(n = 1) %>%
#   #    dplyr::ungroup() %>%
#   #    # make time bins
#   #    dplyr::filter(dispersaltime %in% seq(0,1800,5)) %>%
#   #    dplyr::mutate(dispersalbin = as.numeric(as.character((cut(dispersaltime, breaks = seq(0, 1800, 60), labels = seq(1, 1800, 60), include.lowest = TRUE))))-1) %>%
#   #    dplyr::arrange(id, dispersaltime) %>%
#   #    dplyr::group_by(id, dispersalbin) %>%
#   #    dplyr::summarise(do_union = FALSE) %>%
#   #    sf::st_cast("MULTILINESTRING") %>%
#   # #   # drop intersections
#   #    filter(st_is_valid(geometry))
#
#   maxdispersal <-  ceiling(max(seed_particles_input$dispersaltime) / 60) * 60
#
#   particle_rainbow_points <- seed_particles_input %>%
#     dplyr::filter(dispersaltime %in% seq(0, maxdispersal, 10)) %>%
#     # dplyr::mutate(
#     #   dispersalbin = as.numeric(as.character(
#     #     cut(
#     #       dispersaltime,
#     #       breaks = seq(0, max(seed_particles_input$dispersaltime) + 60, 60),
#     #       labels = seq(0, max(seed_particles_input$dispersaltime), 60),
#     #       include.lowest = TRUE
#     #     )
#     #   ))
#
#     #) %>%
#     dplyr::arrange(id) %>% # Ensure data is sorted by id for lag function
#     dplyr::group_by(id, competency) %>%
#     dplyr::mutate(
#       geometry_lagged = dplyr::lag(geometry, default = NA)
#     ) %>%
#     dplyr::slice(-1) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(
#       geometry_lagged = ifelse(is.na(geometry_lagged), geometry, geometry_lagged),  # Ensure no NA geometries
#       line = sf::st_sfc(purrr::map2(
#         .x = geometry,
#         .y = geometry_lagged,
#         .f = ~ sf::st_cast(sf::st_union(c(.x, .y)), "LINESTRING")
#       ))
#     ) %>%
#     dplyr::select(id, line, dispersaltime)
#
#
# particle_rainbow <- particle_rainbow_points |>
#     sf::st_sf(geometry = sf::st_sfc(particle_rainbow_points$line, crs = sf::st_crs(particle_rainbow_points))) |>
#     dplyr::arrange(id, dispersaltime) |> dplyr::select(-id)
#
#
#   #cat("3 \n")
#
#
#   seascape_color_pal <- c(
#     "Plateau" = "cornsilk2",
#     "Back Reef Slope" = "darkcyan",
#     "Reef Slope" = "darkseagreen4",
#     "Sheltered Reef Slope" = "darkslategrey",
#     "Inner Reef Flat" = "darkgoldenrod4",
#     "Outer Reef Flat" = "darkgoldenrod2",
#     "Reef Crest" = "coral3"#,
#  #   "Deep Lagoon" = "transparent",
#  #   "Shallow Lagoon" = "transparent"
#   ) %>% rlang::set_names(names(.))
#
#
#   restoration_plot <- seed_particles_input |>
#     dplyr::filter(dispersaltime==0) |>
#     dplyr::slice(1) |>
#     sf::st_centroid() |>
#     coralseed::set_restoration_plot(restoration.plot[1], restoration.plot[2], crs=st_crs(seed_particles_input))
#
#
#   #cat("4 \n")
#
#   tmap::tmap_mode("view")
#
#   if (webGL==TRUE){
#     tmap::tm_view(use_WebGL=TRUE)
#   }
#
#   tmp <- tm_basemap("Esri.WorldImagery") +
#
#     # seascape habitats
#
#     tmap::tm_shape(seascape_probability, name = "<b> [Seascape]</b> habitats") +
#     tmap::tm_polygons(fill="class",
#                       lwd = 0.2, col="black",
#                       fill_alpha = 0.6,
#                       fill.legend = tmap::tm_legend(title = "Benthic habitats"),
#                       fill.scale = tmap::tm_scale_categorical(values = seascape_color_pal)) +
#
#
#     # seascape habitats
#     tmap::tm_shape(seascape_probability, name = "<b> [Seascape]</b> probability") +
#     tmap::tm_polygons(fill="settlement_probability",
#                       lwd = 0.2, col="black",
#                       fill_alpha = 0.6,
#                       lty.legend  = tmap::tm_legend(title = "Settlement probability"),
#                       fill.scale = tmap::tm_scale_continuous(values="brewer.reds")) +
#
#     # particle tracks competency
#     tmap::tm_shape(particletracks, name = "<b> [Particles]</b> competency") +
#     tmap::tm_lines("competency",
#                    lwd = 0.8,
#                    lty.legend  = tmap::tm_legend(title = "Competency"),
#                    tmap::tm_scale_categorical(values= c("lightblue", "cadetblue4"))) +
#
#     # # particle tracks
#     # if (nrow(particle_paths) > 0) {
#     # tmap::tm_shape(particletracks, name =  "<b> [Particles]</b> dispersal tracks") +
#     # tmap::tm_lines(col="grey",
#     #                lwd = 0.8,
#     #                lty.legend  = tmap::tm_legend(title = "Dispersal tracks")) +
#     # }
#
#     # pre-settlement tracks
#     tmap::tm_shape(particle_paths, name =  "<b> [Settlers]</b> pre-settlement tracks") +
#     tmap::tm_lines(col="grey",
#                    lwd = 0.8,
#                    lty.legend  = tmap::tm_legend(title = "Settled larval tracks")) +
#
#     # kde2d heatmap
#     tmap::tm_shape(heatmap_coralseed, name = "<b> [Stats]</b> settlement heatmap") +
#     tmap::tm_raster(
#                     col.scale = tmap::tm_scale_continuous(values = "gn_bu"),
#                     col.legend = tmap::tm_legend_hide(),
#                     col_alpha = tmap::tm_const()
#                     ) +
#
#     #particle points
#     tmap::tm_shape(particle_points, is.main=TRUE, id="dispersaltime2", name = "<b> [Settlers]</b> post-settlement location") +
#     tmap::tm_dots(fill="dispersaltime2", shape=21, col="black",
#                   fill.scale = tmap::tm_scale_continuous(values="-brewer.spectral"),
#                   fill.legend = tmap::tm_legend("Dispersal time \n(hours)")) +
#
#     # post-=settlement area
#     tmap::tm_shape(settlement_density_input$area, name = "<b> [Settlers]</b> restoration footprint") +
#     tmap::tm_dots(fill="coral", fill_alpha = 0.3) +
#
#     # # settlement grid
#     tmap::tm_shape(settlement_density_input$density, name = "<b> [Stats]</b> spatial grid") +
#     tmap::tm_borders(lwd = 0.15,
#                      col = "black") +
#     #
#     # # settlement count
#     tmap::tm_shape(settlement_density_input$count, name = "<b> [Stats]</b> settlement count") +
#     tmap::tm_polygons(fill="count",
#                       lwd = 0,
#                       fill_alpha = 0.6,
#                       fill.scale = tmap::tm_scale_continuous(values="-plasma"),
#                       fill.legend = tmap::tm_legend("Settlement count")) +
#
#     # settlement density
#     tmap::tm_shape(settlement_density_input$density, name = "<b> [Stats]</b> settlement density") +
#     tmap::tm_polygons(fill="density",
#                       lwd=0,
#                       fill_alpha = 0.6,
#                       fill.scale = tmap::tm_scale_continuous(values="-magma"),
#                       fill.legend = tmap::tm_legend("Settlement density")) +
#
#     # restoration plot
#     tmap::tm_shape(restoration_plot, name = "<b> [Stats]</b> restoration hectare") +
#     tmap::tm_borders(lwd = 2,
#                      col = "red") +
#
#     # tmap options
#     tmap::tmap_options(#check.and.fix = TRUE,
#                        show.messages=FALSE,
#                        show.warnings=FALSE) +
#
#     tmap::tm_credits("https://github.com/marine-ecologist/coralseed/")
#
#   if(show.tracks==TRUE){
#
#   tmp <- tmp +
#
#     tmap::tm_shape(particle_rainbow,
#                    name = "<b> [Particles]</b> dispersaltime") +
#     tmap::tm_lines(col="dispersaltime",
#                    lwd = 0.8,
#                    tmap::tm_scale_continuous(values = "-brewer.spectral"),
#                    col.legend = tmap::tm_legend(title = "Dispersal time"))
#
#
#   }
#
#   if (!is.null(scalebar)){
#   tmp <-  tmp +
#     tmap::tm_scalebar(width = scalebar)
#         }
#
#   if (show.footprint==TRUE){
#   tmp <- tmp +
#   # post-settlement area
#   tmap::tm_shape(settlement_density_input$area, id="area", name = "<b> [Settlers]</b> post-settlement area") +
#     tmap::tm_polygons(lwd = 0.8,
#                       fill="indianred4",
#                       fill_alpha=0.6,
#                       fill.legend = tmap::tm_legend(title = "Restoration footprint"))
#
#   }
#
#   tmp |> tmap::tmap_leaflet() |>
#     leaflet::addProviderTiles('Esri.WorldImagery', group = "<b> [Seascape]</b> satellite map", options=leaflet::providerTileOptions(maxNativeZoom=18,maxZoom=100)) |>
#     leaflet::addProviderTiles('Esri.WorldTopoMap',  group = "<b> [Seascape]</b> base map", options=leaflet::providerTileOptions(maxNativeZoom=18,maxZoom=100)) |>
#     leaflet::addLayersControl(position="topleft", overlayGroups=c("<b> [Seascape]</b> satellite map",
#                                                                   "<b> [Seascape]</b> habitats",
#                                                                   "<b> [Seascape]</b> probability",
#                                                                   "<b> [Particles]</b> dispersal tracks",
#                                                                   "<b> [Particles]</b> dispersaltime",
#                                                                   "<b> [Particles]</b> competency",
#                                                                   "<b> [Settlers]</b> pre-settlement tracks",
#                                                                   "<b> [Settlers]</b> post-settlement area",
#                                                                   "<b> [Settlers]</b> post-settlement location",
#                                                                   "<b> [Stats]</b> spatial grid",
#                                                                   "<b> [Stats]</b> settlement count",
#                                                                   "<b> [Stats]</b> settlement density",
#                                                                   "<b> [Stats]</b> settlement heatmap",
#                                                                   "<b> [Stats]</b> restoration hectare",
#                                                                   "<b> [Settlers]</b> restoration footprint"),
#                               options=leaflet::layersControlOptions(collapsed = FALSE)) |>
#     leaflet::hideGroup(c("<b> [Seascape]</b> base map",
#                          "<b> [Seascape]</b> probability",
#                          "<b> [Particles]</b> dispersaltime",
#                          "<b> [Particles]</b> competency",
#                          "<b> [Particles]</b> dispersal tracks",
#                          "<b> [Settlers]</b> post-settlement area",
#                          "<b> [Stats]</b> spatial grid",
#                          "<b> [Stats]</b> settlement count",
#                          "<b> [Stats]</b> settlement heatmap",
#                          "<b> [Stats]</b> settlement density",
#                          "<b> [Settlers]</b> restoration footprint")) |>
#     leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)
#
#
# }
