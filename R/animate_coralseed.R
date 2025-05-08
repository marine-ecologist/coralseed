#' Animate Coral Seed Particle Tracks
  #'
  #' This function animates particle tracks over time using tmap, creating a .gif file.
  #'
  #' @param input An sf object containing particle tracks. Must include a 'time' and 'id' column.
  #' @param filename Path to the output gif file.
  #' @param width Width of the output gif in pixels. Default is 1200.
  #' @param height Height of the output gif in pixels. Default is 600.
  #' @param delay Delay between frames in the gif, in milliseconds. Default is 10.
  #' @param loop Logical; should the gif loop? Default is FALSE.
  #'
  #' @details
  #' Packages required: \code{tmap}, \code{sf}, \code{dplyr}
  #'
  #' @return Saves a gif to the specified filename and returns the animation object.
  #' @export
  #'
  #' @examples
  #' # library(sf)
  #' # library(dplyr)
  #' # library(tmap)
  #' #
  #' # sf::sf_use_s2(FALSE)
  #' # lizard_particles_raw <- sf::st_read("/path/to/file.json", quiet=TRUE) |>
  #' #   dplyr::filter(id < 100) |>
  #' #   sf::st_zm(drop = TRUE, what = "ZM")
  #' #
  #' # animate_coralseed(
  #' #   input = lizard_particles_raw,
  #' #   filename = "/path/to/save/test.gif"
  #' # )
  #'
  animate_coralseed <- function(input, filename, width = 1200, height = 600, delay = 10, loop = FALSE) {

        animobj <- tmap::tm_shape(
            input |>
                dplyr::arrange(time, id) |>
                dplyr::mutate(order = as.factor(time)),
            bbox = sf::st_bbox(input)
          )
            tmap::tm_symbols(fill = "time", size = 0.1)
            tmap::tm_facets("order", nrow = 1, ncol = 1)

          anim <- tmap::tmap_animation(
              animobj,
              filename = filename,
              loop = loop,
              width = width,
              height = height,
              delay = delay
            )

            return(anim)
        }
