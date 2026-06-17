# Seed particles

Function to convert xarrr arrays from oceanparticles to df format

## Usage

``` r
import_zarr(input, origin = "1970-01-01 15:00:00", crs = 4326)
```

## Arguments

- input:

  input zarr folder

- origin:

  POSIXct origin datetime string (default "1970-01-01 15:00:00")

- crs:

  coordinate reference system (default = 4326)

## Details

zarr_file_path \<-
"/Users/rof011/oceanparcels/outputs/Elford_16073_Slope_71a.zarr/"
