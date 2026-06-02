#' Process NetCDF rasters for a given year
#'
#' This function processes a directory of NetCDF files by extracting a specified
#' variable (surface level only), cropping/masking to a polygon, and optionally
#' downsampling to a target resolution. It outputs a single combined raster for
#' the year and writes it to a temporary GeoTIFF file.
#'
#' @param year_dir Character. Path to the directory containing `.nc` files for a single year.
#' @param polygon An `sf` polygon object. Used for cropping and/or masking rasters.
#' @param crop Logical. If `TRUE`, crops rasters to the extent of `polygon`.
#' @param mask Logical. If `TRUE`, masks rasters to the interior of `polygon`.
#' @param downsample Logical. If `TRUE`, resamples rasters to a coarser grid of resolution `res`.
#' @param res Numeric (length 1 or 2). Target resolution for downsampling (map units of CRS).
#' @param variable Character. Variable name prefix to extract from NetCDF (e.g. `"sst"`).
#'                 The function appends `"_zlev=0"` to match surface data layers.
#' @param preliminary Logical. If `TRUE`, allows processing of files flagged as preliminary.
#'                    If `FALSE` (default), the function will stop if preliminary data are found.
#'
#' @return Character. Path to the combined yearly raster written to a temporary GeoTIFF,
#' or `NULL` if no valid rasters were processed.
#'
#' @details
#' The function:
#' 1. Scans `year_dir` for `.nc` files.
#' 2. Skips or replaces filenames with `_preliminary.nc` depending on `preliminary`.
#' 3. Extracts the specified variable at surface depth (`zlev=0`).
#' 4. Applies optional crop/mask/downsample steps.
#' 5. Combines rasters and writes out a GeoTIFF file in the session temp directory.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' poly <- st_read("polygon.gpkg")
#' tif <- process_year("data/1998", poly, crop = TRUE, mask = TRUE,
#'                     downsample = TRUE, res = 0.1, variable = "sst", preliminary = FALSE)
#' raster <- terra::rast(tif)
#' }



process_year <- function(year_dir, polygon, crop, mask, downsample, res, variable, preliminary) {
  rlist <- base::list.files(path = year_dir, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)

  if (!preliminary && any(grepl("_preliminary\\.nc$", rlist))) {
    stop("Includes preliminary data. Remove and re-run or flag preliminary = TRUE.")
  }

  if (preliminary) {
    rlist <- gsub("_preliminary\\.nc$", ".nc", rlist)
  }

  processed_rasters <- list()
  for (file in rlist) {
    base::cat("Reading file:", file, "\n")
    r <- try(terra::rast(file), silent = TRUE)
    if (inherits(r, "try-error")) next

    varname <- paste0(variable, "_zlev=0")
    if (!(varname %in% names(r))) {
      base::cat("Skipping:", file, " — variable not found\n")
      next
    }

    r <- r[[varname]]
    base::names(r) <- base::as.Date(terra::time(r))
    poly_t <- terra::vect(sf::st_transform(polygon, terra::crs(r)))
    if (isTRUE(mask)) r <- terra::mask(r, poly_t)
    if (isTRUE(crop)) r <- terra::crop(r, poly_t)
    if (isTRUE(downsample)) {
      target <- terra::rast(terra::ext(r), resolution = res, crs = terra::crs(r))
      r <- terra::resample(r, target, method = "bilinear")
    }
    processed_rasters <- base::c(processed_rasters, r)
    base::cat("Processed:", file, "\n")
  }

  if (length(processed_rasters) == 0) return(NULL)

  year_combined <- base::do.call(c, processed_rasters)
  tempfile_name <- base::file.path(tempdir(), paste0("year_", basename(year_dir), ".tif"))
  terra::writeRaster(year_combined, filename = tempfile_name, overwrite = TRUE)
  return(tempfile_name)
}
