#' @title Summarise settlement results
  #' @description Computes summary statistics on larval settlement outcomes including spatial footprint, distance travelled, density, and spatial distribution metrics.
  #' @param seeded_particles List containing seed_particles data frame with settlement time.
  #' @param settled_particles List containing settled particles with `$points` (sf POINT) and `$paths` (distance info).
  #' @param cellsize Numeric grid resolution used for spatial footprint calculation.
  #' @param ... Additional arguments passed to settlement_statistics().
  #' @return A data frame of summary statistics.
  #' @importFrom sf st_coordinates st_bbox st_area
  #' @importFrom dplyr pull
  #' @importFrom tidyr drop_na
  #' @importFrom spatstat.geom ppp as.owin nndist
  #' @importFrom spatstat.explore clarkevans
  #' @importFrom stats median
  #' @export
  #'
  settlement_summary <- function(seeded_particles, settled_particles, cellsize, ...) {

        settled_n <- nrow(settled_particles$points)
        settlement_success <- (settled_n / length(unique(seeded_particles$seed_particles$id))) * 100
        mean_settlement_time <- round(median(seeded_particles$seed_particles$settlement_point) / 60, 1)
        range_settlement_time <- round(max(seeded_particles$seed_particles$settlement_point) / 60, 1)
        median_distance <- mean(settled_particles$paths$distance) |> round(1)
        range_distance <- max(settled_particles$paths$distance) |> round(1)

          tmp <- settlement_statistics(settled_particles$points, concavehull = TRUE)
          footprint <- tmp$footprint |> sf::st_area()
          areacovered <- tmp$grid |> tidyr::drop_na() |> nrow()
          mediangrid <- tmp$grid |> tidyr::drop_na() |> dplyr::pull(count) |> median()
          rangegrid <- tmp$grid |> tidyr::drop_na() |> dplyr::pull(count) |> max()
          densitygrid <- tmp$grid |> tidyr::drop_na() |> dplyr::pull(density) |> median()

            ppp_obj <- spatstat.geom::ppp(
                x = sf::st_coordinates(settled_particles$points)[, 1],
                y = sf::st_coordinates(settled_particles$points)[, 2],
                window = spatstat.geom::as.owin(sf::st_bbox(settled_particles$points))
              )

              nn_dists <- mean(spatstat.geom::nndist(ppp_obj))
              clark <- spatstat.explore::clarkevans(ppp_obj)

                col1 <- c(
                    "1) Settlement success:",
                    "How many larvae settled?",
                    "What percent of released larvae settled?",
                    "2) Time to settlement",
                    "What was the average time to settlement?",
                    "What was the min/max time to settlement?",
                    "3) Larval distance",
                    "How far on average did larvae travel before settling?",
                    "What is the max of dispersal distances?",
                    "4) Dispersal footprint",
                    "What is the total spatial footprint of the restoration?",
                    "How many gridcells were seeded within the area?",
                    "What is the median count per gridcell?",
                    "What is the max counts per gridcell?",
                    "What is the median density?",
                    "5) Spatial pattern",
                    "How far apart are the settlers?",
                    "How clustered are the settlers?"
                  )

                  col2 <- c(
                      "-",
                      settled_n,
                      settlement_success,
                      "-",
                      mean_settlement_time,
                      range_settlement_time,
                      "-",
                      median_distance,
                      range_distance,
                      "-",
                      footprint,
                      areacovered,
                      mediangrid,
                      rangegrid,
                      densitygrid,
                      "-",
                      nn_dists,
                      round(as.numeric(clark[3]), 2)
                    )

                    col3 <- c(
                        "-",
                        "Total number of larvae settled",
                        "Percent settlement success",
                        "-",
                        "Median settlement (hrs)",
                        "Min/max settlement (hrs)",
                        "-",
                        "Distance (meters)",
                        "Distance (meters)",
                        "-",
                        "Meters ^2",
                        paste0("n grids (",cellsize, "*", cellsize,",m)"),
                        paste0("count (",cellsize, "*", cellsize,",m)"),
                        paste0("count (",cellsize, "*", cellsize,",m)"),
                        paste0("density (",cellsize, "*", cellsize,",m)"),
                        "-",
                        "Nearest-neighbour distance (metres)",
                        "Clark-Evans index"
                      )

                      df_output <- data.frame(Description = col1, Value = col2, Units = col3)
                      return(df_output)
                    }
