#' Map coralseed settlers subfunction
#'
#' Function to quickly map settlers for multiple
#'
#'
#' @param points input from seed_particles
#' @param paths input from settle_particles
#' @param seascape_probability input from seascape_probability
#' @param restoration.plot dimensions of the restoration plot in metres
#' @param cellsize grid cell width/height
#' @param concavity a relative measure of concavity. 1 results in a relatively detailed shape, Infinity results in a convex hull. You can use values lower than 1, but they can produce pretty crazy shapes.
#' @param length_threshold when a segment length is under this threshold, it stops being considered for further detalization. Higher values result in simpler shapes.
#' @export
#'
map_coralseed_future <- function(points = output_points, paths = paths,
                                 seascape_probability = seascape, restoration.plot = c(100, 100),
                                 cellsize=10, concavity=1.2, length_threshold=10) {

  # restoration_plot <- points |> set_restoration_plot_futures(100, 100)


  settled_particles_grid <- sf::st_make_grid(dplyr::select(points,
                                                           -id, -class, -time, -cat), cellsize = cellsize, what = "polygons")
  grid_count <- sp::over(sf::as_Spatial(settled_particles_grid),
                         sf::as_Spatial(dplyr::select(points, id, class)), fn = length)
  settled_particles_count <- dplyr::mutate(sf::st_as_sf(settled_particles_grid),
                                           count = as.numeric(tidyr::replace_na(grid_count$id, NA)))
  settled_particles_density <- dplyr::mutate(settled_particles_count,
                                             density = count/(cellsize * cellsize))
  settled_particles_concavehull <- concaveman::concaveman(points |> select(geometry),
                                                          concavity = concavity, length_threshold = length_threshold)
  settled_particles_concavehull$area <- round(sf::st_area(settled_particles_concavehull))

  tmap::tmap_mode("view")
  tmp <- tmap::tm_view() +


    tmap::tm_tiles("Esri.WorldImagery", group = "<b> [Seascape]</b> satellite map", alpha = 0.85) +
    tmap::tm_tiles("Esri.WorldTopoMap", group = "<b> [Seascape]</b> base map", alpha = 0.85) +

    tmap::tm_shape(seascape_probability |> sf::st_make_valid(), name = "<b> [Seascape]</b> habitats") +
      tmap::tm_fill("class", title="Habitat type", name = "Benthic habitats", palette = c("Plateau" = "cornsilk2", "Back Reef Slope" = "darkcyan", "Reef Slope" = "darkseagreen4", "Sheltered Reef Slope" = "darkslategrey", "Inner Reef Flat" = "darkgoldenrod4", "Outer Reef Flat" = "darkgoldenrod2", "Reef Crest" = "coral3"), alpha = 0.3) +
      tmap::tm_borders(col = "black", lwd = 0.2) +

    #tmap::tm_shape(points , id="dispersaltime", name = "<b> [Settlers]</b> settlement points", is.master=TRUE) +
    #  tmap::tm_dots(col = "aquamarine3") +

    tmap::tm_shape(settled_particles_concavehull, is.master=TRUE, id="area", name = "<b> [Settlers]</b> post-settlement area") +
      tmap::tm_fill(title="Spatial footprint", "polygons", col = "orange",  alpha = 0.6) +
      tmap::tm_borders(lwd = 0, col = "black") +

    tmap::tm_shape(settled_particles_grid, name = "<b> [Stats]</b> spatial grid") +
      tmap::tm_borders(lwd = 0.5, col = "black") +

    tmap::tm_shape(settled_particles_count, name = "<b> [Stats]</b> settlement count") +
      tmap::tm_fill("count", title="Settlers per cell", colorNA = "transparent", name = "Settlement density", palette = "YlGnBu", alpha = 0.8) +
      tmap::tm_borders(lwd = 0, col = "black") +

    tmap::tm_shape(settled_particles_density, name = "<b> [Stats]</b> settlement density") +
      tmap::tm_fill("density", title="Settlers per m2", colorNA = "transparent", name = "Settlement density", palette = "YlGnBu", alpha = 0.8) +
      tmap::tm_borders(lwd = 0, col = "black")  +

    # tmap::tm_shape(restoration_plot, name = "<b> [Stats]</b> restoration hectare") +
    # tmap::tm_borders(lwd = 2, col = "darkred") +

    tmap::tmap_options(check.and.fix = TRUE, show.warnings = FALSE)

  tmp |> tmap::tmap_leaflet() |>
    leaflet::addProviderTiles('Esri.WorldTopoMap',  group = "<b> [Seascape]</b> base map", options=leaflet::providerTileOptions(maxNativeZoom=19,maxZoom=100)) |>
    leaflet::addProviderTiles('Esri.WorldImagery', group = "<b> [Seascape]</b> satellite map", options=leaflet::providerTileOptions(maxNativeZoom=18,maxZoom=100)) |>
    leaflet::addLayersControl(position="topleft", overlayGroups=c("<b> [Seascape]</b> base map", "<b> [Seascape]</b> satellite map",
      "<b> [Seascape]</b> habitats",  #"<b> [Settlers]</b> settlement points",
      "<b> [Settlers]</b> post-settlement area", "<b> [Stats]</b> spatial grid",
      "<b> [Stats]</b> spatial grid", "<b> [Stats]</b> settlement count", "<b> [Stats]</b> settlement density"),
      options=leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::hideGroup(c("<b> [Seascape]</b> base map", #"<b> [Seascape]</b> satellite map",
                         "<b> [Seascape]</b> habitats",  #"<b> [Settlers]</b> settlement points",
                         "<b> [Settlers]</b> post-settlement area", #"<b> [Stats]</b> spatial grid", "<b> [Stats]</b> settlement density",
                          "<b> [Stats]</b> settlement count"))

}

