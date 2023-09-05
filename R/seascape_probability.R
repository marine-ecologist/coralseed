#' Seascape Probability
#'
#' Function to generate probability distribution from Allen Coral Atlas input files
#'
#' @param reefoutline geojson or shp files for Coral Atlas benthic habitat maps for reef outline
#' @param geomorphic habitat or shp files for Coral Atlas geomorphic maps
#' @param probability habitat specific probabilities (mean and SE) defined as in example below. Defaults to NULL parameterisation embedded in function (see example below)
#' @param ... passes functions
#' @export



seascape_probability <- function(reefoutline = NULL, habitat = NULL, probability = NULL, ...) {
  
  options(dplyr.summarise.inform = FALSE)
  if (is.null(probability) == TRUE) {
    probability <- data.frame(
      class = c("Back Reef Slope", "Lagoon", "Inner Reef Flat", "Outer Reef Flat", "Plateau", "Reef Crest", "Reef Slope", "Sheltered Reef Slope"),
      means = c(0.6, 0, 0.3, 0.45, 0.2, 0.9, 0.9, 0.7),
      se = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
    )
  } else {
    probability <- probability
  }

  tryCatch({

    # 1) Coral Atlas shp files (add load later)
     # load(system.file("data/benthic_map.rda", package = "coralseed"))
      benthic_map <- benthic_map |>
        #dplyr::mutate(class = as.factor(class)) |>
        #dplyr::mutate(class = forcats::fct_recode(class, Lagoon = "Shallow Lagoon", Lagoon = "Deep Lagoon")) |>
        sf::st_transform(crs = sf::st_crs("EPSG:20353"))

      #load(system.file("data/reef_map.rda", package = "coralseed"))
      reef_map <- reef_map |>
        #dplyr::mutate(class = as.factor(class)) |>
        #dplyr::filter(!class %in% c("Microalgal Mats", "Seagrass", "Sand")) |>
        #sf::st_union() |>
        sf::st_transform(crs = sf::st_crs("EPSG:20353"))


    oldwarning <- getOption("warn")
    options(warn = -1)
    allen_map <- sf::st_intersection(benthic_map, reef_map) |>
      sf::st_collection_extract("POLYGON") |>
      dplyr::group_by(class) |>
      dplyr::mutate(habitat_id = paste0(
        gsub(" ", "_", class), "_",
        sprintf(paste0("%0", ceiling(log10(max(1:length(class)))), "d"), 1:length(class))
      )) |>
      sf::st_make_valid()
    options(warn = oldwarning)

    benthic_probability <- probability
    class_means <- rlang::set_names(benthic_probability$means, benthic_probability$class)

    allen_map_probability <- allen_map |>
      dplyr::filter(!class == "Lagoon") |>
      sf::st_make_valid() |>
      dplyr::mutate(settlement_probability = dplyr::recode(class, !!!class_means)) |>
      sf::st_make_valid() |>
      dplyr::left_join(benthic_probability, by = "class") |>
      dplyr::group_by(class) |>
      dplyr::mutate(settlement_probability = round(rnorm(length(class), means, se), 2)) |>
      dplyr::mutate(settlement_probability = ifelse(settlement_probability > 1,
        round(settlement_probability), settlement_probability
      )) |>
      dplyr::mutate(settlement_probability = ifelse(settlement_probability < 0,
        0, settlement_probability
      )) |>
      dplyr::select(-means, -se) |>
      sf::st_make_valid()
  })
  options(dplyr.summarise.inform = TRUE)
  return(allen_map_probability)
}
