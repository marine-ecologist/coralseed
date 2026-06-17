# Simulate Lagrangian particle trajectories from gridded flow fields

Simulates passive particle dispersal from one or more release sites
using gridded eastward and northward velocity fields. Particles are
released over a specified release window, advected using the nearest
available flow-field grid cell, and moved with an added random-walk
diffusion term. Candidate particle positions that intersect land are
repeatedly redrawn up to a maximum retry limit.

## Usage

``` r
simulate_particles(
  flow_field,
  land_poly = NULL,
  release_sites,
  crs_m = 32755,
  dt = 5 * 60,
  release_duration_seconds = 60 * 60,
  release_by = paste(dt, "sec"),
  release_start = NULL,
  n_particles_per_site_per_release = 100,
  K = 0.05,
  max_land_retry = 200,
  seed = 101,
  parallel = FALSE,
  workers = NULL
)
```

## Arguments

- flow_field:

  An `sf` point object containing gridded flow fields. Must contain
  columns `time`, `x`, `y`, `u_ms`, and `v_ms`. Velocities should be in
  metres per second.

- land_poly:

  An `sf` polygon object representing land or other exclusion areas that
  particles should not enter. Use `NULL` to disable land exclusion.

- release_sites:

  An `sf` point object giving particle release locations.

- crs_m:

  Numeric or character. Projected coordinate reference system used for
  particle movement in metres. Default is `32755`.

- dt:

  Numeric. Particle advection time step in seconds. Default is `5 * 60`.

- release_duration_seconds:

  Numeric. Duration over which particles are released, in seconds.
  Default is `60 * 60`.

- release_by:

  Character. Interval between release times, passed to
  [`seq.POSIXt()`](https://rdrr.io/r/base/seq.POSIXt.html). Default is
  `paste(dt, "sec")`, so release times align with flow-field time steps.

- release_start:

  Optional release start time. If `NULL`, the first available flow-field
  timestamp is used.

- n_particles_per_site_per_release:

  Integer. Number of particles released per site at each release time.
  Default is `100`.

- K:

  Numeric. Horizontal diffusivity used in the random-walk term, in
  square metres per second. Default is `0.05`.

- max_land_retry:

  Integer. Maximum number of redraw attempts for particles that
  intersect land after movement. Default is `200`.

- seed:

  Integer. Random seed for reproducible particle release and diffusion.
  Default is `101`.

- parallel:

  Logical. If `TRUE`, run release sites in parallel using `future` and
  `furrr`. Default is `FALSE`.

- workers:

  Integer. Number of parallel workers. If `NULL`, uses one fewer than
  the number of available cores.

## Value

A named list with:

- particle_tracks:

  A data frame containing all particle positions through time.

- particle_tracks_sf:

  An `sf` point object of all particle positions in `crs_m`.

- particle_tracks_4326:

  An `sf` point object of all particle positions transformed to
  EPSG:4326.

- final_particles:

  An `sf` point object containing the final recorded position of each
  particle in `crs_m`.

- final_particles_4326:

  Final particle positions transformed to EPSG:4326.

- particle_track_summary:

  A one-row summary table of particle counts, track rows, and land-retry
  diagnostics.

- particles0:

  Initial particle release table.

- release_sites_m:

  Release sites transformed to `crs_m`, with release coordinates.

- land_m:

  Land polygon transformed to `crs_m`, or `NULL` if `land_poly = NULL`.

## Details

The time loop is sequential because each particle position depends on
its previous position. Optional parallel processing is therefore
implemented by splitting independent simulations across release sites.

Particle movement follows:

\$\$x\_{t + 1} = x_t + u_t dt + \epsilon_x\$\$

\$\$y\_{t + 1} = y_t + v_t dt + \epsilon_y\$\$

where `u_t` and `v_t` are the nearest-neighbour flow velocities and
\\\epsilon_x\\ and \\\epsilon_y\\ are normally distributed random-walk
terms with standard deviation:

\$\$\sqrt{2 K dt}\$\$

Particles that intersect `land_poly` after movement are redrawn from the
previous position using the same advective and diffusive step. If a
particle still intersects land after `max_land_retry` attempts, it is
returned to its previous position for that time step.

Flow fields are matched exactly by timestamp. For this reason,
`release_by = paste(dt, "sec")` is recommended unless the flow field has
finer temporal resolution.

## Examples

``` r
if (FALSE) { # \dontrun{
particle_sim <- simulate_particles(
  flow_field = flow_sim$flow_field,
  land_poly = land_poly,
  release_sites = release_sites,
  crs_m = 32755,
  dt = 5 * 60,
  release_duration_seconds = 60 * 60,
  n_particles_per_site_per_release = 100,
  K = 0.05,
  max_land_retry = 200,
  seed = 101,
  parallel = TRUE,
  workers = 6
)

particle_sim$particle_tracks_sf
particle_sim$final_particles_4326
particle_sim$particle_track_summary
} # }
```
