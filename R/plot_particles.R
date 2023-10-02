#' Plot output from settle_particles
#'
#' Function to plot output of seed_particles in base R for quick cheks
#'
#'
#'
#' @param input input
#' @param parameter parameter (column from coralseed)
#' @param habitatmap basemap from seascape_probability
#' @param size size
#' @param pch pch
#' @param palette one of rainbow(n), heat.colors(n), terrain.colors(n), topo.colors(n), cm.colors(n)
#' @param ... passes functions
#' @export
#' @examples
#' # example code
#' # particles <- seed_particles("mermaid", seascape=seascape, limit_time=720, competency.function = "exponential", simulate.mortality = "typeI", simulate.mortality.n = 0.1, probability="additive",return.plot=FALSE, silent=TRUE)
#' # plot_particles(particles)


plot_particles <- function(input = NULL, seascape=NULL, parameter = "dispersaltime", pch=16, size=0.5, ...) {
  
  if (parameter %in% "dispersaltime") {
    plot(input[parameter], type="l", lwd=0.1, col="white", graticule = TRUE, axes = TRUE, main="", reset = FALSE)
    plot(seascape["class"], lwd=0.1, col = sf.colors(categorical=TRUE, alpha = 0.25), add=TRUE)
    plot(input["id"], type="l", lwd=0.1, col="grey", main="", add=TRUE)
    plot(input[(parameter)], type="p", cex=0.5, pch=16, main="", add=TRUE, key.pos = 1)
    
  } else {
    print("One of: 'dispersaltime'")
  }
}


