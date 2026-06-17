# Process NetCDF rasters for a given year

This function processes a directory of NetCDF files by extracting a
specified variable (surface level only), cropping/masking to a polygon,
and optionally downsampling to a target resolution. It outputs a single
combined raster for the year and writes it to a temporary GeoTIFF file.

## Usage

``` r
process_year(
  year_dir,
  polygon,
  crop,
  mask,
  downsample,
  res,
  variable,
  preliminary
)
```

## Arguments

- year_dir:

  Character. Path to the directory containing `.nc` files for a single
  year.

- polygon:

  An `sf` polygon object. Used for cropping and/or masking rasters.

- crop:

  Logical. If `TRUE`, crops rasters to the extent of `polygon`.

- mask:

  Logical. If `TRUE`, masks rasters to the interior of `polygon`.

- downsample:

  Logical. If `TRUE`, resamples rasters to a coarser grid of resolution
  `res`.

- res:

  Numeric (length 1 or 2). Target resolution for downsampling (map units
  of CRS).

- variable:

  Character. Variable name prefix to extract from NetCDF (e.g. `"sst"`).
  The function appends `"_zlev=0"` to match surface data layers.

- preliminary:

  Logical. If `TRUE`, allows processing of files flagged as preliminary.
  If `FALSE` (default), the function will stop if preliminary data are
  found.

## Value

Character. Path to the combined yearly raster written to a temporary
GeoTIFF, or `NULL` if no valid rasters were processed.

## Details

The function:

1.  Scans `year_dir` for `.nc` files.

2.  Skips or replaces filenames with `_preliminary.nc` depending on
    `preliminary`.

3.  Extracts the specified variable at surface depth (`zlev=0`).

4.  Applies optional crop/mask/downsample steps.

5.  Combines rasters and writes out a GeoTIFF file in the session temp
    directory.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
poly <- st_read("polygon.gpkg")
tif <- process_year("data/1998", poly, crop = TRUE, mask = TRUE,
                    downsample = TRUE, res = 0.1, variable = "sst", preliminary = FALSE)
raster <- terra::rast(tif)
} # }
```
