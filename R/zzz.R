.onLoad <- function(libname, pkgname) {
  if (!exists("infamis_tiles_exp", envir = parent.env(environment()))) {
    data("infamis_tiles_exp", package = pkgname, envir = parent.env(environment()))
  }
}
