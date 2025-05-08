.onLoad <- function(libname, pkgname) {
  # This ensures that if infamis_tiles_exp does not exist,
  # it is loaded from the .rda file in the data/ directory.
  if (!exists("infamis_tiles_exp", envir = parent.env(environment()))) {
    data("infamis_tiles_exp", package = pkgname, envir = parent.env(environment()))
  }
}
