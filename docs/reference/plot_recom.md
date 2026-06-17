# plot_recom: Visualize Ocean Currents and Wind from NetCDF Data

Reads a NetCDF file containing ocean current and wind data, extracts
relevant variables at a specified depth (-2.35m), and generates
time-series and spatial plots. Highlights slack current time and
visualizes magnitude and direction of currents and wind.

## Usage

``` r
plot_recom(
  nc_file = "/Users/rof011/GBR_connectivity/Oceanparcels/Moore_uv_2015-12-05.nc",
  parcels_reefs = sf::st_read("/Users/rof011/GBR_connectivity/inputs/Moore_2D.gpkg",
    quiet = TRUE),
  parcels_polygon =
    dplyr::filter(sf::st_read("/Users/rof011/GBR_connectivity/inputs/Moore_2D.gpkg",
    quiet = TRUE), site_id == "Moore_16071_Slope_39a")
)
```

## Arguments

- nc_file:

  Character. Path to the NetCDF file containing oceanographic data.

- parcels_reefs:

  sf object. Spatial dataset of reef locations.

- parcels_polygon:

  sf object. Spatial dataset for a specific study site polygon.

## Value

A combined ggplot object displaying temporal trends of ocean currents
and wind, along with spatial maps of current speeds at the slack current
time.
