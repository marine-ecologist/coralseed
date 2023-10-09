#' Silence ggplot warnings
#'
#' @description Replacement for \code{print.ggplot()} that mutes warnings.
#'
#' @param plot_obj input
#' @param ...  Arguments are passed to \code{print.ggplot()}
#' @importFrom ggplot2 ggplot
#' @export
ggplot_silent <- function(plot_obj, ...) {
  suppressWarnings(print(plot_obj))
}
