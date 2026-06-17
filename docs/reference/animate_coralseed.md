# Animate Coral Seed Particle Tracks

This function animates particle tracks over time using tmap, creating a
.gif file.

## Usage

``` r
animate_coralseed(
  input,
  filename,
  width = 1200,
  height = 600,
  delay = 10,
  loop = FALSE
)
```

## Arguments

- input:

  An sf object containing particle tracks. Must include a 'time' and
  'id' column.

- filename:

  Path to the output gif file.

- width:

  Width of the output gif in pixels. Default is 1200.

- height:

  Height of the output gif in pixels. Default is 600.

- delay:

  Delay between frames in the gif, in milliseconds. Default is 10.

- loop:

  Logical; should the gif loop? Default is FALSE.

## Value

Saves a gif to the specified filename and returns the animation object.

## Details

Packages required: `tmap`, `sf`, `dplyr`

## Examples

``` r
# library(sf)
# library(dplyr)
# library(tmap)
#
# sf::sf_use_s2(FALSE)
# lizard_particles_raw <- sf::st_read("/path/to/file.json", quiet=TRUE) |>
#   dplyr::filter(id < 100) |>
#   sf::st_zm(drop = TRUE, what = "ZM")
#
# animate_coralseed(
#   input = lizard_particles_raw,
#   filename = "/path/to/save/test.gif"
# )
```
