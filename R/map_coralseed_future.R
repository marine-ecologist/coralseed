#' Map coralseed settlers subfunction
#'
#' Function to quickly map settlers for multiple
#'
#'
#' @param seed_particles input from seed_particles
#' @param settle_particles input from settle_particles
#' @param seascape_probability input from seascape_probability
#' @param restoration.plot dimensions of the restoration plot in metres
#' @export
#'
map_coralseed_future <- function(points = points, paths = paths, seascape_probability = seascape, restoration.plot = c(100, 100)) {

  settler_density <- points |> settlement_density()
  
  settler_centroid <- dplyr::summarize(points, geometry = sf::st_union(geometry))
    x <- sf::st_coordinates(settler_centroid)[1, 1]
    y <- sf::st_coordinates(settler_centroid)[1, 2]
    x_min <- x - (restoration.plot[1]/2)
    x_max <- x + (restoration.plot[1]/2)
    y_min <- y - (restoration.plot[2]/2)
    y_max <- y + (restoration.plot[2]/2)
    restoration_plot <- sf::st_sfc(sf::st_polygon(list(rbind(c(x_min, y_min), 
                                                    c(x_min, y_max), 
                                                    c(x_max, y_max), 
                                                    c(x_max, y_min), 
                                                    c(x_min, y_min)))), 
                                                    crs = 20353)
  
  #spatial_boundary <- paths |> set_buffer(1)

  tmap::tmap_mode("view")
  tmp <- tmap::tm_view() +

    tmap::tm_tiles("Esri.WorldImagery", group = "<b> [Seascape]</b> satellite map", alpha = 0.85) +
    tmap::tm_tiles("Esri.WorldTopoMap", group = "<b> [Seascape]</b> base map", alpha = 0.85) +


    tmap::tm_shape(seascape_probability |> sf::st_make_valid(), name = "<b> [Seascape]</b> habitats") +
      tmap::tm_fill("class", name = "Benthic habitats", palette = c("Plateau" = "cornsilk2", "Back Reef Slope" = "darkcyan", "Reef Slope" = "darkseagreen4", "Sheltered Reef Slope" = "darkslategrey", "Inner Reef Flat" = "darkgoldenrod4", "Outer Reef Flat" = "darkgoldenrod2", "Reef Crest" = "coral3"), alpha = 0.6) +
      tmap::tm_borders(col = "black", lwd = 0.2) +

    tmap::tm_shape(points , id="dispersaltime", name = "<b> [Settlers]</b> settlement points", is.master=TRUE) +
      tmap::tm_dots(col = "aquamarine3") +

    tmap::tm_shape(settler_density$area, id="area", name = "<b> [Settlers]</b> post-settlement area") +
      tmap::tm_fill("polygons", col = "indianred4",  alpha = 0.6) +
      tmap::tm_borders(lwd = 0, col = "black") +

    tmap::tm_shape(settler_density$density, name = "<b> [Stats]</b> spatial grid") +
      tmap::tm_borders(lwd = 0.5, col = "black") +

    tmap::tm_shape(settler_density$count, name = "<b> [Stats]</b> settlement count") +
      tmap::tm_fill("count", colorNA = "transparent", name = "Settlement density", palette = "plasma", alpha = 0.6) +
      tmap::tm_borders(lwd = 0, col = "black") +

    tmap::tm_shape(settler_density$density, name = "<b> [Stats]</b> settlement density") +
      tmap::tm_fill("density", colorNA = "transparent", name = "Settlement density", palette = "magma", alpha = 0.6) +
      tmap::tm_borders(lwd = 0, col = "black") 
  
    tmap::tmap_options(show.warnings = FALSE)

  tmp |> tmap::tmap_leaflet() |>
    leaflet::addProviderTiles('Esri.WorldTopoMap',  group = "<b> [Seascape]</b> base map", options=leaflet::providerTileOptions(maxNativeZoom=19,maxZoom=100)) |>
    leaflet::addProviderTiles('Esri.WorldImagery', group = "<b> [Seascape]</b> satellite map", options=leaflet::providerTileOptions(maxNativeZoom=18,maxZoom=100)) |>
    leaflet::addLayersControl(position="topleft", overlayGroups=c("<b> [Seascape]</b> base map", "<b> [Seascape]</b> satellite map",
      "<b> [Seascape]</b> habitats", "<b> [Seascape]</b> probability",
      "<b> [Settlers]</b> settlement points", "<b> [Settlers]</b> post-settlement area", "<b> [Stats]</b> spatial grid",
      "<b> [Stats]</b> spatial grid", "<b> [Stats]</b> settlement count", "<b> [Stats]</b> settlement density", "<b> [Stats]</b> restoration hectare"),
      options=leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::hideGroup(c("<b> [Seascape]</b> probability", "<b> [Seascape]</b> satellite map",
     "<b> [Settlers]</b> settlement points", "<b> [Settlers]</b> post-settlement area",
     "<b> [Stats]</b> spatial grid", "<b> [Stats]</b> settlement count", "<b> [Stats]</b> settlement density", "<b> [Stats]</b> restoration hectare"))

}

