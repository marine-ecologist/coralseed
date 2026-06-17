# Generate a Heatmap of Settler Particles

This function computes a kernel density estimate (KDE) heatmap from
spatial points (settler particles) and returns a raster representation.
The bounding box is expanded by a specified factor to avoid edge
effects.

## Usage

``` r
.settler_heatmap_legacy(
  input,
  xres = 20,
  yres = 20,
  buffer_factor = 0.1,
  n = 100,
  threshold = 10
)
```

## Arguments

- input:

  An sf object containing point geometries representing settler
  particles.

- xres:

  Numeric. The resolution of the heatmap in the x direction meters.
  Default is 20.

- yres:

  Numeric. The resolution of the heatmap in the y direction meters.
  Default is 20.

- buffer_factor:

  Numeric. The proportion of the bounding box range to expand the
  heatmap extent. Default is 0.1 10%.

- n:

  KDE grid resolution

- threshold:

  Numeric. Thresholding value \< will remove low-density values
