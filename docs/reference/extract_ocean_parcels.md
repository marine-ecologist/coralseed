# extract_ocean_particles

Function to convert ocean particles Zarr folders to coralseed format
Extracts metadata to get correct time format uses Rarr:read_zarr_array()
to get lat, lon, time, z arrays converts to sf points

## Usage

``` r
extract_ocean_parcels(folder, crs = 4326, subsample = "none", depth = FALSE)
```

## Arguments

- folder:

  input folder e.g. /outputs.zarr/

- crs:

  output crs for sf object

- subsample:

  sample n from total oceanparcels trajectories

- depth:

  TRUE/FALSE, if FALSE returns 2D array (xy), if TRUE returns 3D array
  (xyz)

## Details

Example usage folder \<-
"/Users/rof011/Mooreoutputs2015Moore_trajectory_2015_d12d.zarr_mon20th.zarr/"
sf_data \<- extract_ocean_parcels(folder, sample=10, crs=20353)
print(sf_data)
