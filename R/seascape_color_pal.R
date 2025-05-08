#' Seascape color palette
#'
#' Returns a named vector of colors for coral seascape habitat classes.
#'
#' @name seascape_color_pal
#' @return A named character vector of colors
#' @export
#'

seascape_color_pal <- function() {
  pal <- c(
    "Plateau" = "cornsilk2",
    "Back Reef Slope" = "darkcyan",
    "Reef Slope" = "darkseagreen4",
    "Sheltered Reef Slope" = "darkslategrey",
    "Inner Reef Flat" = "darkgoldenrod4",
    "Outer Reef Flat" = "darkgoldenrod2",
    "Reef Crest" = "coral3"
    # "Deep Lagoon" = "transparent",
    # "Shallow Lagoon" = "transparent"
  )
  return(rlang::set_names(pal, names(pal)))
}
