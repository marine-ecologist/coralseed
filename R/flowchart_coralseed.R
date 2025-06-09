#' @title Flowchart of Coral Larval Fates
  #' @description Generate an interactive Sankey diagram showing the fate of coral larvae
  #'              through dispersal, settlement, and mortality.
  #'
  #' @param seed_particles_input An `sf` object or tibble containing all released larval trajectories with at least `id`, `dispersaltime`, and `state`.
  #' @param settle_particles_input A named list containing `$points`, an `sf` object with settled larvae and `id`.
  #' @param multiplier Numeric multiplier to scale particle counts (e.g., if each simulated particle represents multiple larvae).
  #' @param postsettlement Postsettlement mortality in proportion (e.g. 80% = 0.8)
  #'
  #' @return An interactive Sankey diagram (htmlwidget).
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' flowchart_coralseed(moore_particles, moore_settlers, multiplier = 1, postsettlement = 0.8)
  #' }
  #'
  #'
  #'

  flowchart_coralseed <- function(seed_particles_input, settle_particles_input, save_output=NULL, multiplier = 1, postsettlement = 0.8, width=10, height=6) {

        if (!is.list(seed_particles_input)) {
            stop("flowchart_coralseed requires seed_particles() to be run with return.summary = FALSE\n")
          }

        max_dispersal_time <- seed_particles_input$seed_particles$dispersaltime |> max()
        total_mortality <- as.numeric(seed_particles_input$summary[6, 2])

          total_released <- seed_particles_input$seed_particles |>
              dplyr::pull(id) |> unique() |> length()

            total_settled <- settle_particles_input$points |>
                dplyr::pull(id) |> unique() |> length()

              total_survived <- total_released - total_mortality
              post_settlement_mortality <- 1 - postsettlement

                pretty_round <- function(x) {
                    prettyNum(formatC(x * multiplier, format = "f", digits = 0), big.mark = ",")
                  }

                  pretty_round_flat <- function(x) {
                      prettyNum(formatC(x, format = "f", digits = 0), big.mark = ",")
                    }

                    nodes <- data.frame(
                        name = c(
                            paste0("Released Larvae [", pretty_round(total_released), "]"),
                            paste0("Settled larvae [", pretty_round(total_settled), "]"),
                            paste0("Dispersed larvae [", pretty_round(total_survived - total_settled), "]"),
                            paste0("Dead larvae [", pretty_round(total_mortality), "]"),
                            paste0("Coral recruits (live)  [", pretty_round(total_settled * post_settlement_mortality), "]"),
                            paste0("Dead recruits (post-settlement mortality) [", pretty_round(total_settled * (1 - post_settlement_mortality)), "]")
                          ),
                        group = c("source", "settled", "dispersed", "dead", "alive", "post")
                      )

                      links <- data.frame(
                          source = c(0, 0, 0, 1, 1),
                          target = c(1, 2, 3, 4, 5),
                          value = c(
                              total_settled,
                              total_survived - total_settled,
                              total_mortality,
                              total_settled * post_settlement_mortality,
                              total_settled * (1 - post_settlement_mortality)
                            ),
                          group = c("settled", "dispersed", "dead", "alive", "post")
                        )

                        my_color <- 'd3.scaleOrdinal()
    .domain(["source", "settled", "dispersed", "dead", "alive", "post"])
    .range(["#ffffff", "#90C8C6", "#AEDCA9", "#FEF491", "#7AAAA8", "#FEF491"])'

                          n_tracks <- total_released / as.numeric(multiplier)

                            caption_text <- paste0(
                                "[Total particles ", pretty_round(total_released),
                                " | n tracks ", pretty_round(n_tracks),
                                " | Larvae per track = ", pretty_round_flat(multiplier),
                                " | Maximum dispersal time = ", max_dispersal_time, " minutes]"
                              )

                              p <- networkD3::sankeyNetwork(
                                  Links = links,
                                  Nodes = nodes,
                                  Source = "source",
                                  Target = "target",
                                  Value = "value",
                                  NodeID = "name",
                                  NodeGroup = "group",
                                  LinkGroup = "group",
                                  colourScale = my_color,
                                  sinksRight = FALSE,
                                  fontSize = 12,
                                  fontFamily = "arial",
                                  nodeWidth = 40,
                                  nodePadding = 25
                                )

                                p <- htmlwidgets::onRender(p, "
    function(el, x) {
      d3.select(el).selectAll('.link')
        .style('stroke-opacity', 0.3);
    }
  ")

                                  if (!is.null(save_output)) {
                                      # Create temporary HTML
                                        temp_html <- tempfile(fileext = ".html")

                                          # Save widget properly
                                          htmlwidgets::saveWidget(
                                              widget = p,
                                              file = temp_html,
                                              selfcontained = TRUE,
                                              libdir = NULL,
                                              background = "white"
                                            )

                                          # Now correct webshot
                                          ext <- tools::file_ext(save_output)

                                            if (ext %in% c("png", "jpg", "jpeg")) {
                                                webshot2::webshot(
                                                    url = temp_html,
                                                    file = save_output,
                                                    vwidth = width,
                                                    vheight = height,
                                                    delay = 0.5 # give time to render fully
                                                  )
                                              } else if (ext == "pdf") {
                                                  webshot2::webshot(
                                                      url = temp_html,
                                                      file = save_output,
                                                      vwidth = width,
                                                      vheight = height,
                                                      delay = 0.5,
                                                      zoom = 2
                                                    )
                                                } else {
                                                    stop("Unsupported output format: must be .png, .jpg, or .pdf")
                                                  }
                                        }


                                  return(htmltools::browsable(
                                      htmltools::tagList(
                                          htmltools::tags$h5(
                                              caption_text,
                                              style = "text-align:center; font-family:arial; color:#9e9e9e;"
                                            ),
                                          p
                                        )
                                    ))
                              }




# flowchart_coralseed <- function(seed_particles_input, settle_particles_input,
#                                 save_output = NULL, multiplier = 1,
#                                 postsettlement = 0.8, width = 10, height = 6) {
#
#   if (!is.list(seed_particles_input)) {
#     stop("flowchart_coralseed requires seed_particles() to be run with return.summary = FALSE\n")
#   }
#
#   max_dispersal_time <- max(seed_particles_input$seed_particles$dispersaltime)
#   total_mortality <- as.numeric(seed_particles_input$summary[16, 2])
#   total_released <- length(unique(seed_particles_input$seed_particles$id))
#   total_settled <- length(unique(settle_particles_input$points$id))
#   total_survived <- total_released - total_mortality
#   post_settlement_mortality <- 1 - postsettlement
#
#   pretty_round <- function(x) prettyNum(formatC(x * multiplier, format = "f", digits = 0), big.mark = ",")
#   pretty_round_flat <- function(x) prettyNum(formatC(x, format = "f", digits = 0), big.mark = ",")
#
#   nodes <- data.frame(
#     name = c(
#       paste0("Released Larvae [", pretty_round(total_released), "]"),
#       paste0("Settled larvae [", pretty_round(total_settled), "]"),
#       paste0("Dispersed larvae [", pretty_round(total_survived - total_settled), "]"),
#       paste0("Dead larvae [", pretty_round(total_mortality), "]"),
#       paste0("Coral recruits (live)  [", pretty_round(total_settled * post_settlement_mortality), "]"),
#       paste0("Dead recruits (post-settlement mortality) [", pretty_round(total_settled * (1 - post_settlement_mortality)), "]")
#     ),
#     group = c("source", "settled", "dispersed", "dead", "alive", "post")
#   )
#
#   links <- data.frame(
#     source = c(0, 0, 0, 1, 1),
#     target = c(1, 2, 3, 4, 5),
#     value = c(
#       total_settled,
#       total_survived - total_settled,
#       total_mortality,
#       total_settled * post_settlement_mortality,
#       total_settled * (1 - post_settlement_mortality)
#     ),
#     group = c("settled", "dispersed", "dead", "alive", "post")
#   )
#
#   my_color <- 'd3.scaleOrdinal()
#     .domain(["source", "settled", "dispersed", "dead", "alive", "post"])
#     .range(["#ffffff", "#90C8C6", "#AEDCA9", "#FEF491", "#7AAAA8", "#FEF491"])'
#
#   n_tracks <- total_released / as.numeric(multiplier)
#
#   caption_text <- paste0(
#     "[Total particles ", pretty_round(total_released),
#     " | n tracks ", pretty_round(n_tracks),
#     " | Larvae per track = ", pretty_round_flat(multiplier),
#     " | Maximum dispersal time = ", max_dispersal_time, " minutes]"
#   )
#
#   p <- networkD3::sankeyNetwork(
#     Links = links,
#     Nodes = nodes,
#     Source = "source",
#     Target = "target",
#     Value = "value",
#     NodeID = "name",
#     NodeGroup = "group",
#     LinkGroup = "group",
#     colourScale = my_color,
#     sinksRight = FALSE,
#     fontSize = 12,
#     fontFamily = "arial",
#     nodeWidth = 40,
#     nodePadding = 25
#   )
#
#   p <- htmlwidgets::onRender(p, "
#     function(el, x) {
#       d3.select(el).selectAll('.link')
#         .style('stroke-opacity', 0.3);
#     }
#   ")
#
#   if (!is.null(save_output)) {
#     temp_html <- tempfile(fileext = ".html")
#
#     htmlwidgets::saveWidget(
#       widget = p,
#       file = temp_html,
#       selfcontained = TRUE,
#       libdir = NULL,
#       background = "white"
#     )
#
#     ext <- tools::file_ext(save_output)
#     if (ext %in% c("png", "jpg", "jpeg")) {
#       webshot2::webshot(
#         url = temp_html,
#         file = save_output,
#         vwidth = width,
#         vheight = height,
#         delay = 0.5
#       )
#     } else if (ext == "pdf") {
#       webshot2::webshot(
#         url = temp_html,
#         file = save_output,
#         vwidth = width,
#         vheight = height,
#         delay = 0.5,
#         zoom = 2
#       )
#     } else {
#       stop("Unsupported output format: must be .png, .jpg, or .pdf")
#     }
#   }
#
#   htmltools::browsable(
#     htmltools::tagList(
#       htmltools::tags$h5(
#         caption_text,
#         style = "text-align:center; font-family:arial; color:#9e9e9e;"
#       ),
#       p
#     )
#   )
# }
#
# #
# # flowchart_coralseed <- function(seed_particles_input, settle_particles_input, multiplier = 1, postsettlement = 0.8) {
# #
# #   if (!is.list(seed_particles_input)) {
# #     stop("flowchart_coralseed requires seed_particles() to be run with return.summary = FALSE\n")
# #   }
# #
# #   max_dispersal_time <- seed_particles_input$seed_particles$dispersaltime |> max()
# #   total_mortality <- as.numeric(seed_particles_input$summary[16, 2])
# #
# #   total_released <- seed_particles_input$seed_particles |>
# #     dplyr::pull(id) |> unique() |> length()
# #
# #   total_settled <- settle_particles_input$points |>
# #     dplyr::pull(id) |> unique() |> length()
# #
# #   total_survived <- total_released - total_mortality
# #   post_settlement_mortality <- 1 - postsettlement
# #
# #   pretty_round <- function(x) {
# #     prettyNum(formatC(x * multiplier, format = "f", digits = 0), big.mark = ",")
# #   }
# #
# #   nodes <- data.frame(
# #     name = c(
# #       paste0("Released Larvae [", pretty_round(total_released), "]"),
# #       paste0("Settled larvae [", pretty_round(total_settled), "]"),
# #       paste0("Dispersed larvae [", pretty_round(total_survived - total_settled), "]"),
# #       paste0("Dead larvae [", pretty_round(total_mortality), "]"),
# #       paste0("Coral recruits (live)  [", pretty_round(total_settled * post_settlement_mortality), "]"),
# #       paste0("Dead recruits (post-settlement mortality) [", pretty_round(total_settled * (1 - post_settlement_mortality)), "]")
# #     ),
# #     group = c("source", "settled", "dispersed", "dead", "alive", "post")
# #   )
# #
# #   links <- data.frame(
# #     source = c(0, 0, 0, 1, 1),
# #     target = c(1, 2, 3, 4, 5),
# #     value = c(
# #       total_settled,
# #       total_survived - total_settled,
# #       total_mortality,
# #       total_settled * post_settlement_mortality,
# #       total_settled * (1 - post_settlement_mortality)
# #     ),
# #     group = c("settled", "dispersed", "dead", "alive", "post")
# #   )
# #
# #   my_color <- 'd3.scaleOrdinal()
# #     .domain(["source", "settled", "dispersed", "dead", "alive", "post"])
# #     .range(["#ffffff", "#90C8C6", "#AEDCA9", "#FEF491", "#7AAAA8", "#FEF491"])'
# #
# #   caption_text <- paste0(
# #     "[Particle tracks = ", pretty_round(total_released),
# #     " | Larvae per track = ", multiplier,
# #     " | Maximum dispersal time = ", max_dispersal_time, " minutes]"
# #   )
# #
# #   p <- networkD3::sankeyNetwork(
# #     Links = links,
# #     Nodes = nodes,
# #     Source = "source",
# #     Target = "target",
# #     Value = "value",
# #     NodeID = "name",
# #     NodeGroup = "group",
# #     LinkGroup = "group",
# #     colourScale = my_color,
# #     sinksRight = FALSE,
# #     fontSize = 12,
# #     fontFamily = "arial",
# #     nodeWidth = 40,
# #     nodePadding = 25
# #   )
# #
# #   p <- htmlwidgets::onRender(p, "
# #     function(el, x) {
# #       d3.select(el).selectAll('.link')
# #         .style('stroke-opacity', 0.3);
# #     }
# #   ")
# #
# #   return(htmltools::browsable(
# #     htmltools::tagList(
# #       htmltools::tags$h5(
# #         caption_text,
# #         style = "text-align:center; font-family:arial; color:#9e9e9e;"
# #       ),
# #       p
# #     )
# #   ))
# # }
