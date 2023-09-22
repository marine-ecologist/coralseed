#' Map coralseed settlers
#'
#' Function to quickly map settlers
#'
#' @name extract_parallel
#' @param seed_particles input from seed_particles
#' @param settle_particles input from settle_particles
#' @param seascape_probability input from seascape_probability
#' @param restoration.plot dimensions of the restoration plot in metres
#' @param show.tracks option to show particle tracks (TRUE, will be large files and slower renders) or without tracks (FALSE, smaller files and faster renders)
#' @export
#'
#'


  # particle_rainbow <- seed_particles |> 
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


map_coralseed <- function(seed_particles = particles, settle_particles = settlers, seascape_probability = seascape, restoration.plot = c(100, 100), show.tracks=TRUE) {
  
  
  particletracks <- seed_particles |>  dplyr::filter(dispersaltime %in% seq(0,1800,5)) |>  particles_to_tracks(by=c("id", "competency")) |> select(-id)
  settler_density <- settle_particles |> settlement_density()
  restoration_plot <- particles |> set_restoration_plot(100, 100) 
  
  particle_paths <- rbind(settle_particles$paths, settle_particles$paths[1:117,])
  particle_points <- settle_particles$points
  

  # particle_rainbow <- seed_particles |> 
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
  
  particle_rainbow_form <- particles |> 
    dplyr::filter(dispersaltime %in% seq(0,1800,5)) %>%
    dplyr::mutate(dispersalbin = as.numeric(as.character((cut(dispersaltime, breaks = seq(0, 1800, 60), labels = seq(1, 1800, 60), include.lowest = TRUE))))-1) %>%
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
        .f = ~{st_union(c(.x, .y)) %>% st_cast("LINESTRING")}
      ))) %>% 
    dplyr::select(id, line, dispersalbin)
  
  particle_rainbow <- particle_rainbow_form  |> 
    sf::st_sf(geometry = sf::st_sfc(particle_rainbow_form$line, crs = sf::st_crs(particle_rainbow_form))) |> 
    dplyr::arrange(id, dispersalbin) |> select(-id)
  
  if(show.tracks==TRUE){
  tmp <- tmap::tm_view() +
    
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

    # particle tracks dispersal
    tmap::tm_shape(particle_rainbow, name = "<b> [Particles]</b> dispersaltime") +
    tmap::tm_lines("dispersalbin", lwd = 0.8, id="dispersalbin", palette = "-Spectral", type="cont", breaks=seq(0,ceiling(max(particles$dispersaltime/60))*60,60)) +
    
    # particle tracks competency
    tmap::tm_shape(particletracks, name = "<b> [Particles]</b> competency") +
    tmap::tm_lines("competency", lwd = 0.8, id="competency", palette = c("lightblue", "cadetblue4")) +
    
    # pre-settlement tracks
    tmap::tm_shape(particle_paths,  id="dispersaltime", name = "<b> [Settlers]</b> pre-settlement tracks") +
    tmap::tm_lines(lwd = 0.8, col = "darkgrey") +
    
    # particle points
    tmap::tm_shape(particle_points, is.master=TRUE, id="dispersaltime", name = "<b> [Settlers]</b> post-settlement location") +
    tmap::tm_dots("dispersaltime", palette="-Spectral") +
    
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
    tmap::tmap_options(check.and.fix = TRUE, show.messages=FALSE, show.warnings=FALSE) 
  
  
  tmp |> tmap::tmap_leaflet() |>
    leaflet::addProviderTiles('Esri.WorldImagery', group = "<b> [Seascape]</b> satellite map", options=leaflet::providerTileOptions(maxNativeZoom=18,maxZoom=100)) |>
    leaflet::addProviderTiles('Esri.WorldTopoMap',  group = "<b> [Seascape]</b> base map", options=leaflet::providerTileOptions(maxNativeZoom=19,maxZoom=100)) |>
    leaflet::addLayersControl(position="topleft", overlayGroups=c("<b> [Seascape]</b> base map", "<b> [Seascape]</b> satellite map",
                                                                  "<b> [Seascape]</b> habitats", "<b> [Seascape]</b> probability",
                                                                  "<b> [Particles]</b> dispersaltime", "<b> [Particles]</b> competency",
                                                                  "<b> [Settlers]</b> pre-settlement tracks", "<b> [Settlers]</b> post-settlement location", 
                                                                  "<b> [Settlers]</b> post-settlement area",
                                                                  "<b> [Stats]</b> spatial grid", "<b> [Stats]</b> settlement count",
                                                                  "<b> [Stats]</b> settlement density", "<b> [Stats]</b> restoration hectare"),
                              options=leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::hideGroup(c("<b> [Seascape]</b> probability",  
                         "<b> [Particles]</b> dispersaltime","<b> [Particles]</b> competency",
                         #"<b> [Settlers]</b> pre-settlement tracks", "<b> [Settlers]</b> post-settlement location",
                         "<b> [Settlers]</b> post-settlement area", "<b> [Stats]</b> spatial grid",
                         "<b> [Stats]</b> settlement count", "<b> [Stats]</b> settlement density")) |>
    leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)
} else if (show.tracks==FALSE){
  
  tmp <- tmap::tmap_mode("view") +
    
    #---------- seascape habitats -----------------@
    tmap::tm_shape(seascape_probability, name = "<b> [Seascape]</b> habitats") +
    tmap::tm_borders(col = "black", lwd = 0.2) +
    tmap::tm_fill("class", name = "Benthic habitats", palette = c("Plateau" = "cornsilk2", "Back Reef Slope" = "darkcyan",
                                                                  "Reef Slope" = "darkseagreen4", "Sheltered Reef Slope" = "darkslategrey",
                                                                  "Inner Reef Flat" = "darkgoldenrod4", "Outer Reef Flat" = "darkgoldenrod2",
                                                                  "Reef Crest" = "coral3"), alpha = 0.6) +
    #---------- seascape habitats -----------------@
    tmap::tm_shape(seascape_probability, name = "<b> [Seascape]</b> probability") +
    tmap::tm_borders(col = "black", lwd = 0.2) +
    tmap::tm_fill("settlement_probability", id = "P settlement", alpha = 0.6) +
  

    #---------- pre-settlement tracks -----------------@
    tmap::tm_shape(particle_paths,  id="id", name = "<b> [Settlers]</b> pre-settlement tracks") +
    tmap::tm_lines(lwd = 0.8, col = "darkgrey") +
    
    #---------- particle points -----------------@
    tmap::tm_shape(particle_points, is.master=TRUE, id="dispersaltime", name = "<b> [Settlers]</b> post-settlement location") +
    tmap::tm_dots(col = "aquamarine3") +
    
    #---------- post-settlement area -----------------@
    tmap::tm_shape(settler_density$area, id="area", name = "<b> [Settlers]</b> post-settlement area") +
    tmap::tm_fill("polygons", col = "indianred4",  alpha = 0.6) +
    tmap::tm_borders(lwd = 0, col = "black") +
    
    #---------- settlement grid -----------------@
    tmap::tm_shape(settler_density$density, name = "<b> [Stats]</b> spatial grid") +
    tmap::tm_borders(lwd = 0.5, col = "black") +
    
    #---------- settlement count -----------------@
    tmap::tm_shape(settler_density$count, name = "<b> [Stats]</b> settlement count") +
    tmap::tm_fill("count", colorNA = "transparent", name = "Settlement density", palette = "plasma", alpha = 0.6) +
    tmap::tm_borders(lwd = 0, col = "black") +
    
    #----------  settlement density -----------------@
    tmap::tm_shape(settler_density$density, name = "<b> [Stats]</b> settlement density") +
    tmap::tm_fill("density", colorNA = "transparent", name = "Settlement density", palette = "magma", alpha = 0.6) +
    tmap::tm_borders(lwd = 0, col = "black") +
    
    #---------- restoration plot -----------------@
    tmap::tm_shape(restoration_plot, name = "<b> [Stats]</b> restoration hectare") +
    tmap::tm_borders(lwd = 2, col = "red") +
    
    # tmap options    
    tmap::tmap_options(check.and.fix = TRUE, show.messages=FALSE, show.warnings=FALSE) 
  
  
  tmp |> tmap::tmap_leaflet() |>
    leaflet::leafletOptions(preferCanvas = TRUE) |> 
    leaflet::addProviderTiles('Esri.WorldImagery', group = "<b> [Seascape]</b> satellite map", options=leaflet::providerTileOptions(maxNativeZoom=18,maxZoom=100)) |>
    leaflet::addProviderTiles('Esri.WorldTopoMap',  group = "<b> [Seascape]</b> base map", options=leaflet::providerTileOptions(maxNativeZoom=19,maxZoom=100)) |>
    leaflet::addLayersControl(position="topleft", overlayGroups=c("<b> [Seascape]</b> base map", "<b> [Seascape]</b> satellite map",
                                                                  "<b> [Seascape]</b> habitats", "<b> [Seascape]</b> probability", 
                                                                  "<b> [Settlers]</b> pre-settlement tracks", 
                                                                  "<b> [Settlers]</b> post-settlement location", 
                                                                  "<b> [Settlers]</b> post-settlement area", 
                                                                  "<b> [Stats]</b> spatial grid", "<b> [Stats]</b> settlement count",
                                                                  "<b> [Stats]</b> settlement density", "<b> [Stats]</b> restoration hectare"),
                              options=leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::hideGroup(c("<b> [Seascape]</b> probability",
                         "<b> [Settlers]</b> post-settlement area", "<b> [Stats]</b> spatial grid", 
                         "<b> [Stats]</b> settlement count", "<b> [Stats]</b> settlement density")) |> 
    leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)  
  
  

}
}