#' Plot output from settle_particles
#'
#' Function to plot output of seed_particles in base R for quick cheks
#'
#'
#'
#' @param input input
#' @param size size
#' @param pch pch
#' @param ... passes functions
#' @export
#' @examples
#' \dontrun{
#' #' # particles <- seed_particles("mermaid", seascape=seascape, limit_time=720, competency.function = "exponential", simulate.mortality = "typeI", simulate.mortality.n = 0.1, probability="additive",return.plot=FALSE, silent=TRUE)
#' # plot_particles(particles)
#'}

plot_settlers <- function(input = NULL, pch=16, size=0.5, ...) {
  palette <- heat.colors(12)
  sfpoints <- input$points
  sfpaths <- input$paths

    tmax <- ceiling(max(sfpoints$dispersaltime))
    breaks <- seq(0,tmax, 30)
    palette_breaks <- rev(heat.colors(length(breaks) - 1))
    col_index <- findInterval(sfpoints$dispersaltime, breaks)
    mapped_colors <- palette_breaks[col_index]

    plot(sfpoints["id"], type="l", lwd=0.1, col="white", graticule = TRUE, axes = TRUE, main="", reset = FALSE)
    plot(seascape["class"], lwd=0.1, col = sf.colors(categorical=TRUE, alpha = 0.25), add=TRUE)
    plot(sfpaths["id"], type="l", lwd=0.2, col="grey", main="", add=TRUE)
    plot(sfpoints["dispersaltime"], type="p", bg=mapped_colors, color="black", cex=1, pch=21, main="", add=TRUE, graticule=TRUE)

    }


