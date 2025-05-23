% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_ocean_parcels.R
\name{extract_ocean_parcels}
\alias{extract_ocean_parcels}
\title{extract_ocean_particles}
\usage{
extract_ocean_parcels(folder, crs = 4326, subsample = "none", depth = FALSE)
}
\arguments{
\item{folder}{input folder e.g. /outputs.zarr/}

\item{crs}{output crs for sf object}

\item{subsample}{sample n from total oceanparcels trajectories}

\item{depth}{TRUE/FALSE, if FALSE returns 2D array (xy), if TRUE returns 3D array (xyz)}
}
\description{
Function to convert ocean particles Zarr folders to coralseed format
Extracts metadata to get correct time format
uses Rarr:read_zarr_array() to get lat, lon, time, z arrays
converts to sf points
}
\details{
Example usage
folder <- "/Users/rof011/Mooreoutputs2015Moore_trajectory_2015_d12d.zarr_mon20th.zarr/"
sf_data <- extract_ocean_parcels(folder, sample=10, crs=20353)
print(sf_data)
}
