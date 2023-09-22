#' Silence ggplot warnings
#' 
#' @description replacement for ggplot() call that mutes warnings
#'
#' @param ...  arguments are passed to \code{ggplot2:::print.ggplot()}
#' @import ggplot2 call
#' @export

ggplot_silent <- function(...) {
  suppressWarnings(ggplot2:::print.ggplot(...))
}
