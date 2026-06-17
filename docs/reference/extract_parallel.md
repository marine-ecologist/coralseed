# Color palettes

Functions to colour maps

Function to extract points and paths from multiple %dopar% outputs

Function to quickly map settlers

## Usage

``` r
seascapecolors()

extract_parallel(input, type)

map_coralseed(
  seed_particles_input = NULL,
  settle_particles_input = NULL,
  settlement_density_input = NULL,
  crs = 20353,
  seascape_probability = NULL,
  restoration.plot = c(100, 100),
  show.tracks = TRUE,
  show.footprint = FALSE,
  subsample = NULL,
  heatmap_res = 2,
  heatmap_buffer = 0.25,
  scalebar = 200,
  webGL = FALSE
)
```

## Arguments

- input:

  list of %dopar% outputs

- type:

  either points or paths

- seed_particles_input:

  input from seed_particles_input

- settle_particles_input:

  input from settle_particles_input

- settlement_density_input:

  input from settlement density

- crs:

  coordinate reference system (default 20353)

- seascape_probability:

  input from seascape_probability

- restoration.plot:

  dimensions of the restoration plot in metres

- show.tracks:

  option to show particle tracks (TRUE, will be large files and slower
  renders)

- show.footprint:

  show spatial footprint (TRUE/FALSE)

- subsample:

  subsample large datasets for visualisation (n)

- heatmap_res:

  spatial resolution of heatmap

- heatmap_buffer:

  spatial buffer around heatmap in metres

- scalebar:

  set scale to X metres

- webGL:

  use webGL in tmap? TRUE/FALSE
