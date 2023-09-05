#' Map coralseed settlers
#'
#' Function to quickly map settlers
#'
#'
#' @param seed_particles input from seed_particles
#' @param settle_particles input from settle_particles
#' @param seascape_probability input from seascape_probability
#' @param restoration.plot dimensions of the restoration plot in metres
#' @export
#'
map_coralseed <- function(seed_particles = particles, settle_particles = settlers, seascape_probability = seascape, restoration.plot = c(100, 100)) {

 
  particletracks <- particles |> particles_to_tracks(slicesample=100, by="competency") |> st_make_valid()
  settler_density <- settle_particles |> settlement_density()
  restoration_plot <- seed_particles |> set_restoration_plot(100, 100)
  
  seascape_probability <- seascape
  particle_paths <- settlers$paths
  particle_points <- settlers$points
  
  tmp <- tmap::tmap_mode("view") +
    
    # seascape habitats
    tmap::tm_shape(seascape_probability, name = "<b> [Seascape]</b> habitats") +
    tmap::tm_borders(col = "black", lwd = 0.2) +
    tmap::tm_fill("class", name = "Benthic habitats", palette = c("Plateau" = "cornsilk2", "Back Reef Slope" = "darkcyan",
                                                                  "Reef Slope" = "darkseagreen4", "Sheltered Reef Slope" = "darkslategrey",
                                                                  "Inner Reef Flat" = "darkgoldenrod4", "Outer Reef Flat" = "darkgoldenrod2",
                                                                  "Reef Crest" = "coral3"), alpha = 0.6) +
    # seascape habitats
    tmap::tm_shape(seascape_probability, name = "<b> [Seascape]</b> probability") +
    tmap::tm_borders(col = "black", lwd = 0.2) +
    tmap::tm_fill("settlement_probability", id = "P settlement", alpha = 0.6) +
    
    # particle tracks   
    #tmap::tm_shape(particletracks) +
    #tmap::tm_lines("competency", lwd = 0.8, palette = c("lightblue", "cadetblue4")) +
    
    # pre-settlement tracks   
    tmap::tm_shape(particle_paths,  id="id", name = "<b> [Settlers]</b> pre-settlement tracks") +
    tmap::tm_lines(lwd = 0.8, col = "darkgrey") +
    
    # particle points
    tmap::tm_shape(particle_points, is.master=TRUE, id="dispersaltime", name = "<b> [Settlers]</b> post-settlement location") +
    tmap::tm_dots(col = "aquamarine3") +
    
    # post-settlement area
    tmap::tm_shape(settler_density$area, id="area", name = "<b> [Settlers]</b> post-settlement area") +
    tmap::tm_fill("polygons", col = "indianred4",  alpha = 0.6) +
    tmap::tm_borders(lwd = 0, col = "black") +
    
    # settlement grid
    tmap::tm_shape(settler_density$density, name = "<b> [Stats]</b> spatial grid") +
    tmap::tm_borders(lwd = 0.5, col = "black") +
    
    # settlement count
    
    tmap::tm_shape(settler_density$count, name = "<b> [Stats]</b> settlement count") +
    tmap::tm_fill("count", colorNA = "transparent", name = "Settlement density", palette = "plasma", alpha = 0.6) +
    tmap::tm_borders(lwd = 0, col = "black") +
    
    # settlement density
    tmap::tm_shape(settler_density$density, name = "<b> [Stats]</b> settlement density") +
    tmap::tm_fill("density", colorNA = "transparent", name = "Settlement density", palette = "magma", alpha = 0.6) +
    tmap::tm_borders(lwd = 0, col = "black") +
    
    # restoration plot
    tmap::tm_shape(restoration_plot, name = "<b> [Stats]</b> restoration hectare") +
    tmap::tm_borders(lwd = 2, col = "red") +
    
    # tmap options    
    tmap::tmap_options(check.and.fix = TRUE) 
  
  
  tmp |> tmap::tmap_leaflet() |>
    leaflet::addProviderTiles('Esri.WorldImagery', group = "<b> [Seascape]</b> satellite map", options=leaflet::providerTileOptions(maxNativeZoom=18,maxZoom=100)) |>
    leaflet::addProviderTiles('Esri.WorldTopoMap',  group = "<b> [Seascape]</b> base map", options=leaflet::providerTileOptions(maxNativeZoom=19,maxZoom=100)) |>
    leaflet::addLayersControl(position="topleft", overlayGroups=c("<b> [Seascape]</b> base map", "<b> [Seascape]</b> satellite map",
                                                                  "<b> [Seascape]</b> habitats", "<b> [Seascape]</b> probability", "<b> [Particles]</b> competency",
                                                                  "<b> [Settlers]</b> pre-settlement tracks", "<b> [Settlers]</b> post-settlement location", "<b> [Settlers]</b> post-settlement area", "<b> [Stats]</b> spatial grid",
                                                                  "<b> [Stats]</b> spatial grid", "<b> [Stats]</b> settlement count", "<b> [Stats]</b> settlement density", "<b> [Stats]</b> restoration hectare"),
                              options=leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::hideGroup(c("<b> [Seascape]</b> probability", "<b> [Particles]</b> competency",
                         "<b> [Settlers]</b> pre-settlement tracks", "<b> [Settlers]</b> post-settlement location", "<b> [Settlers]</b> post-settlement area",
                         "<b> [Stats]</b> spatial grid", "<b> [Stats]</b> settlement count", "<b> [Stats]</b> settlement density", "<b> [Stats]</b> restoration hectare")) |> 
    leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)
  
}

