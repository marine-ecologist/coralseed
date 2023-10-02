#' Silence ggplot warnings
#' 
#' @description replacement for \code{ggplot2()} that mutes warnings
#'
#' @param ...  arguments are passed to \code{ggplot2:::print.ggplot()}
#' @import ggplot2
#' @export

ggplot_silent <- function(...) {
  suppressWarnings(ggplot2:::print.ggplot(...))
}
