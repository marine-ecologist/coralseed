#' Seascape Probability
#'
#' Function to generate spatial settlement probability layers from Allen Coral Atlas maps.
#'
#' @param reefoutline `sf` object (or filepath) for Coral Atlas benthic habitat (reef outline)
#' @param habitat `sf` object (or filepath) for Coral Atlas geomorphic classes
#' @param probability Optional data.frame with `class`, `means`, and `se` columns for custom settlement probabilities.
#'                    If NULL, defaults are used.
#' @param ... Not used. Placeholder for extensibility.
#'
#' @return `sf` object with `class`, `habitat_id`, and `settlement_probability`
#' @export
#'

seascape_probability <- function(reefoutline = benthic_map,
                                 habitat = reef_map,
                                 probability = NULL,
                                 ...) {

  oldwarning <- getOption("warn")
  options(dplyr.summarise.inform = FALSE, warn = -1)

  if (is.null(probability)) {
    probability <- data.frame(
      class = c("Back Reef Slope", "Lagoon", "Inner Reef Flat", "Outer Reef Flat",
                "Plateau", "Reef Crest", "Reef Slope", "Sheltered Reef Slope"),
      means = c(0.6, 0, 0.3, 0.45, 0.2, 0.9, 0.9, 0.7),
      se = rep(0.1, 8)
    )
  }

  benthic_map_import <- reefoutline |>
    sf::st_transform(crs = sf::st_crs("EPSG:20353"))

  reef_map_import <- habitat |>
    sf::st_transform(crs = sf::st_crs("EPSG:20353"))

  allen_map <- sf::st_intersection(benthic_map_import, reef_map_import) |>
    sf::st_collection_extract("POLYGON") |>
    dplyr::group_by(class) |>
    dplyr::summarise() |>
    sf::st_cast("POLYGON") |>
    dplyr::mutate(habitat_id = paste0(
      gsub(" ", "_", class), "_",
      sprintf(paste0("%0", ceiling(log10(max(1:length(class)))), "d"), 1:length(class))
    )) |>
    sf::st_make_valid()

  benthic_probability <- probability
  class_means <- rlang::set_names(benthic_probability$means, benthic_probability$class)

  sf::st_agr(allen_map) <- "constant"

  allen_map_probability <- allen_map |>
    dplyr::filter(!grepl("Lagoon", class)) |>
    sf::st_make_valid() |>
    dplyr::mutate(settlement_probability = dplyr::recode(class, !!!class_means)) |>
    sf::st_make_valid() |>
    dplyr::left_join(benthic_probability, by = "class") |>
    dplyr::group_by(class) |>
    dplyr::mutate(settlement_probability = round(rnorm(dplyr::n(), means, se), 2)) |>
    dplyr::mutate(settlement_probability = ifelse(settlement_probability > 1,
                                                  round(settlement_probability),
                                                  settlement_probability)) |>
    dplyr::mutate(settlement_probability = ifelse(settlement_probability < 0,
                                                  0,
                                                  settlement_probability)) |>
    dplyr::select(-means, -se) |>
    sf::st_make_valid()

  options(dplyr.summarise.inform = TRUE, warn = oldwarning)
  return(allen_map_probability)
}



# seascape_probability <- function(reefoutline = benthic_map, habitat = reef_map, probability = NULL, ...) {
#
#   oldwarning <- getOption("warn")
#   options(dplyr.summarise.inform = FALSE, warn = -1)
#   if (is.null(probability) == TRUE) {
#     probability <- data.frame(
#       class = c("Back Reef Slope", "Lagoon", "Inner Reef Flat", "Outer Reef Flat", "Plateau", "Reef Crest", "Reef Slope", "Sheltered Reef Slope"),
#       means = c(0.6, 0, 0.3, 0.45, 0.2, 0.9, 0.9, 0.7),
#       se = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
#     )
#   } else {
#     probability <- probability
#   }
#
#   #tryCatch({
#    # suppressMessages({
#     #  suppressWarnings({
#         #data(benthic_map, envir = environment())
#         #data(reef_map, envir = environment())
#
#         # 1) Coral Atlas shp files (add load later)
#         # load(system.file("data/benthic_map.rda", package = "coralseed"))
#         benthic_map_import <- reefoutline |>
#           # dplyr::mutate(class = as.factor(class)) |>
#           # dplyr::mutate(class = forcats::fct_recode(class, Lagoon = "Shallow Lagoon", Lagoon = "Deep Lagoon")) |>
#           sf::st_transform(crs = sf::st_crs("EPSG:20353"))
#
#         # load(system.file("data/reef_map.rda", package = "coralseed"))
#         reef_map_import <- habitat |>
#           # dplyr::mutate(class = as.factor(class)) |>
#           # dplyr::filter(!class %in% c("Microalgal Mats", "Seagrass", "Sand")) |>
#           # sf::st_union() |>
#           sf::st_transform(crs = sf::st_crs("EPSG:20353"))
#
#         allen_map <- sf::st_intersection(benthic_map_import, reef_map_import) |>
#           sf::st_collection_extract("POLYGON") |>
#           dplyr::group_by(class) |>
#           dplyr::summarise() |>
#           sf::st_cast("POLYGON") |>
#           dplyr::mutate(habitat_id = paste0(
#             gsub(" ", "_", class), "_",
#             sprintf(paste0("%0", ceiling(log10(max(1:length(class)))), "d"), 1:length(class))
#           )) |>
#           sf::st_make_valid()
#
#         benthic_probability <- probability
#         class_means <- rlang::set_names(benthic_probability$means, benthic_probability$class)
#
#         sf::st_agr(allen_map) <- "constant"
#
#
#         allen_map_probability <- allen_map |>
#           dplyr::filter(!grepl("Lagoon", class)) |>
#           sf::st_make_valid() |>
#           dplyr::mutate(settlement_probability = dplyr::recode(class, !!!class_means)) |>
#           sf::st_make_valid() |>
#           dplyr::left_join(benthic_probability, by = "class") |>
#           dplyr::group_by(class) |>
#           dplyr::mutate(settlement_probability = round(rnorm(length(class), means, se), 2)) |>
#           dplyr::mutate(settlement_probability = ifelse(settlement_probability > 1,
#                                                         round(settlement_probability), settlement_probability
#           )) |>
#           dplyr::mutate(settlement_probability = ifelse(settlement_probability < 0,
#                                                         0, settlement_probability
#           )) |>
#           dplyr::select(-means, -se) |>
#           sf::st_make_valid()
#   #     })
#   #   })
#   # }, error = function(e) {
#   #   message("An error occurred: ", e)
#   # })
#   options(dplyr.summarise.inform = TRUE, warn = oldwarning)
#   return(allen_map_probability)
# }
