animate_coralseed <- function(input, outputfolder, n = 100) {

  sf::st_transform(input$paths, 4326) |>
    sf::st_write(paste0(outputfolder, "cesium/geojson/Lpaths.geojson"), driver = "GeoJSON", delete_dsn = TRUE)

  Lparticles <- sf::st_transform(input$lizard_particles$seed_particles, 4326) |>
    dplyr::mutate(time = as.POSIXct(time) + lubridate::hours(10))
  Lpoints <- sf::st_transform(input$points, 4326) |>
    dplyr::mutate(time = as.POSIXct(time) + lubridate::hours(10))

  sf::st_write(Lparticles, paste0(outputfolder, "cesium/geojson/lizard_particles.geojson"), driver = "GeoJSON", delete_dsn = TRUE)
  sf::st_write(Lpoints, paste0(outputfolder, "cesium/geojson/Lpoints.geojson"), driver = "GeoJSON", delete_dsn = TRUE)

  Ldt <- data.table::as.data.table(Lparticles)
  coords <- sf::st_coordinates(Lparticles)
  Ldt$x <- coords[,1]
  Ldt$y <- coords[,2]
  data.table::setorder(Ldt, id, time)

  Ldt[, `:=`(
    x_prev = data.table::shift(x),
    y_prev = data.table::shift(y),
    id_prev = data.table::shift(id)
  )]

  Llines <- Ldt[!is.na(x_prev) & id == id_prev]

  Llines[, linestring := purrr::map2(
    .x = seq_len(.N),
    .y = seq_len(.N),
    ~ sf::st_linestring(matrix(c(x_prev[.x], x[.x], y_prev[.x], y[.x]), ncol = 2))
  )]

  Llines_sf <- sf::st_as_sf(Llines[, .(id, time, linestring)], crs = sf::st_crs(Lparticles))
  names(Llines_sf)[which(names(Llines_sf) == "linestring")] <- "geometry"
  sf::st_geometry(Llines_sf) <- "geometry"

  Llines_sf2 <- Llines_sf |> sf::st_make_valid() |> dplyr::filter(id %in% sample(unique(Llines_sf$id), n))

  Llines2 <- Llines_sf |>
    dplyr::filter(id %in% unique(Lpoints$id)) |>
    dplyr::mutate(interval = lubridate::floor_date(time, unit = "5 minutes")) |>
    dplyr::group_by(id, interval) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::summarise(
      time = max(time),
      geometry = sf::st_combine(geometry),
      .groups = "drop"
    ) |>
    dplyr::mutate(geometry = sf::st_line_merge(geometry)) |>
    sf::st_make_valid()

  sf::st_write(Llines2, paste0(outputfolder, "cesium/geojson/Llines.geojson"), driver = "GeoJSON", delete_dsn = TRUE)
}
