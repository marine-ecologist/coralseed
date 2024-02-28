

particles=particles
settle_particles=settlers

particletracks <- particles |>
  dplyr::filter(dispersaltime %in% seq(0,10000,5)) |> # take 5 minute time slices or paths become very long and complex
  particles_to_tracks(by=c("id", "competency")) |> select(-id)
settler_density <- settlers |> settlement_density()
restoration_plot <- particles |> set_restoration_plot(100, 100)

particle_paths <- rbind(settlers$paths, settlers$paths[1:117,]) # why 1:117?
particle_points <- settlers$points


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
      .f = ~{sf::st_union(c(.x, .y)) %>% sf::st_cast("LINESTRING")}
    ))) %>%
  dplyr::select(id, line, dispersalbin)

particle_rainbow <- particle_rainbow_form  |>
  sf::st_sf(geometry = sf::st_sfc(particle_rainbow_form$line, crs = sf::st_crs(particle_rainbow_form))) |>
  dplyr::arrange(id, dispersalbin)# |> select(-id)


tmap::tmap_mode("plot")


  set.seed(101)
  ids <- sample(particle_points$id, 200)
  particle_points1 <- particle_points  |> filter(id %in% ids)
  particle_paths1 <- particle_paths |> filter(id %in% ids)

 tmp <-  tmap::tm_shape(seascape, name = "<b> [Seascape]</b> habitats") +
    tmap::tm_borders(col = "black", lwd = 0.2) +
    tmap::tm_fill("class", name = "Benthic habitats",  legend.show=FALSE, title="Habitat type", palette = c("Plateau" = "cornsilk2", "Back Reef Slope" = "darkcyan",
                                                                                        "Reef Slope" = "darkseagreen4", "Sheltered Reef Slope" = "darkslategrey",
                                                                                        "Inner Reef Flat" = "darkgoldenrod4", "Outer Reef Flat" = "darkgoldenrod2",
                                                                                        "Reef Crest" = "grey"), alpha = 0.2) +

  # pre-settlement tracks
  # tmap::tm_shape(particle_paths1, is.master=TRUE, id="dispersaltime", name = "<b> [Settlers]</b> pre-settlement tracks") +
  # tmap::tm_lines(lwd = 0.25, col = "darkgrey", title.col="Settled larval tracks") +

  # particle points
  tmap::tm_shape(particle_points1,  id="dispersaltime", name = "<b> [Settlers]</b> post-settlement location") +
  tmap::tm_dots("dispersaltime", shape=21, size=0.1, border.lwd=0.5, alpha=1, palette="-Spectral", title="Dispersal time", legend.show=FALSE) +


    tmap::tm_shape(settler_density$density, name = "<b> [Stats]</b> spatial grid") +
    tmap::tm_borders(lwd = 0.3,  col = "black") #+
  #
  # tmap::tm_shape(st_as_sfc(st_bbox(settler_density$density)) |> st_buffer(200), is.master=TRUE, name = "<b> [Stats]</b> spatial grid") +
  #   tmap::tm_borders(lwd = 0,  col = "black")
  #

 library(lattice)

 counts <- c(18,17,15,20,10,20,25,13,12)
 outcome <- gl(3,1,9)
 treatment <- gl(3,3)
 bwplot <- bwplot(counts ~ outcome | treatment, xlab=NULL, ylab=NULL, cex=.5,
                  scales=list(cex=.5), par.strip.text=list(cex=.5))
 sticker(bwplot, package="hexSticker", p_size=20, s_x=1.05, s_y=.8, s_width=2, s_height=1.5,
         h_fill="#f9690e", h_color="#f39c12", filename="/Users/rof011/coralseed/lattice.pdf")
