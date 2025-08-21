#' Build LINESTRING XYZM trips (integer M from a time column)
#'
#' Minimal helper to create LINESTRING XYZM for mapdeck::add_trips()
#' using an integer range from a time/index column (e.g. "dispersaltime").
#' - type = "points": group points by `group_col` (default "id") ordered by time,
#'   build one LINESTRING per group with M = integer(time).
#' - type = "linestring": if `id_col` present, collapse time-sliced features per id
#'   and ramp M linearly from t -> next_t along vertices; last slice uses next_t = t + 1.
#'   If no `id_col`, each feature ramps t -> t+1.
#'
#' Sets geometry m_range attribute required by mapdeck.
#'
#' @param sf_obj  sf object (POINTS for type="points"; LINESTRING/MULTILINESTRING for type="linestring")
#' @param type    c("points","linestring")
#' @param time_col character name of time/index column (integers or numeric)
#' @param group_col character, group/id column for points (default "id" if present)
#' @param id_col character, id column for linestring collapse (default "id" if present)
#' @param z_val   numeric constant Z (default 0)
#' @param cast_multi logical; cast MULTILINESTRING to LINESTRING (default TRUE)
#' @export
sf_to_xyzm <-  function(sf_obj,
                        time_col = "dispersaltime",
                        z_val = 0,
                        cast_multi = TRUE) {
  stopifnot(inherits(sf_obj, "sf"))
  gcls <- unique(sf::st_geometry_type(sf_obj, by_geometry = TRUE))
  if (!all(gcls %in% c("LINESTRING","MULTILINESTRING"))) stop("geometry must be LINESTRING/MULTILINESTRING")
  if (!time_col %in% names(sf_obj)) stop("time_col not found")

  if (cast_multi && any(gcls == "MULTILINESTRING")) {
    sf_obj <- sf::st_cast(sf_obj, "LINESTRING")
  }

  crs0 <- sf::st_crs(sf_obj)
  tval <- as.numeric(sf_obj[[time_col]])

  geoms <- vector("list", nrow(sf_obj))
  for (i in seq_len(nrow(sf_obj))) {
    xy <- sf::st_coordinates(sf_obj[i, , drop = FALSE])[, 1:2, drop = FALSE]
    if (nrow(xy) < 2) xy <- xy[rep(1, 2), , drop = FALSE]
    mvec <- rep(tval[i], nrow(xy))
    zvec <- rep(z_val,   nrow(xy))
    geoms[[i]] <- sf::st_linestring(cbind(xy, zvec, mvec), dim = "XYZM")
  }

  res <- sf::st_as_sf(sf::st_drop_geometry(sf_obj), geometry = sf::st_sfc(geoms, crs = crs0))
  attr(sf::st_geometry(res), "m_range") <- range(tval, na.rm = TRUE)
  res
}
