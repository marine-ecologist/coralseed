#' Plot output from settle_particles
#'
#' Function to plot output of seed_particles in base R for quick cheks
#'
#'
#'
#' @param input input
#' @param seascape seascape habitat input
#' @param parameter parameter (column from coralseed)
#' @param size size
#' @param pch pch
#' @param ... passes functions
#' @export


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


