#' Simulate Lagrangian particle trajectories from gridded flow fields
#'
#' Simulates passive particle dispersal from one or more release sites using
#' gridded eastward and northward velocity fields. Particles are released over
#' a specified release window, advected using the nearest available flow-field
#' grid cell, and moved with an added random-walk diffusion term. Candidate
#' particle positions that intersect land are repeatedly redrawn up to a
#' maximum retry limit.
#'
#' The time loop is sequential because each particle position depends on its
#' previous position. Optional parallel processing is therefore implemented by
#' splitting independent simulations across release sites.
#'
#' @param flow_field An `sf` point object containing gridded flow fields. Must
#'   contain columns `time`, `x`, `y`, `u_ms`, and `v_ms`. Velocities should be
#'   in metres per second.
#' @param land_poly An `sf` polygon object representing land or other exclusion
#'   areas that particles should not enter. Use `NULL` to disable land
#'   exclusion.
#' @param release_sites An `sf` point object giving particle release locations.
#' @param crs_m Numeric or character. Projected coordinate reference system
#'   used for particle movement in metres. Default is `32755`.
#' @param dt Numeric. Particle advection time step in seconds. Default is
#'   `5 * 60`.
#' @param release_duration_seconds Numeric. Duration over which particles are
#'   released, in seconds. Default is `60 * 60`.
#' @param release_by Character. Interval between release times, passed to
#'   `seq.POSIXt()`. Default is `paste(dt, "sec")`, so release times align with
#'   flow-field time steps.
#' @param release_start Optional release start time. If `NULL`, the first
#'   available flow-field timestamp is used.
#' @param n_particles_per_site_per_release Integer. Number of particles released
#'   per site at each release time. Default is `100`.
#' @param K Numeric. Horizontal diffusivity used in the random-walk term, in
#'   square metres per second. Default is `0.05`.
#' @param max_land_retry Integer. Maximum number of redraw attempts for
#'   particles that intersect land after movement. Default is `200`.
#' @param seed Integer. Random seed for reproducible particle release and
#'   diffusion. Default is `101`.
#' @param parallel Logical. If `TRUE`, run release sites in parallel using
#'   `future` and `furrr`. Default is `FALSE`.
#' @param workers Integer. Number of parallel workers. If `NULL`, uses one fewer
#'   than the number of available cores.
#'
#' @return A named list with:
#' \describe{
#'   \item{particle_tracks}{A data frame containing all particle positions
#'   through time.}
#'   \item{particle_tracks_sf}{An `sf` point object of all particle positions in
#'   `crs_m`.}
#'   \item{particle_tracks_4326}{An `sf` point object of all particle positions
#'   transformed to EPSG:4326.}
#'   \item{final_particles}{An `sf` point object containing the final recorded
#'   position of each particle in `crs_m`.}
#'   \item{final_particles_4326}{Final particle positions transformed to
#'   EPSG:4326.}
#'   \item{particle_track_summary}{A one-row summary table of particle counts,
#'   track rows, and land-retry diagnostics.}
#'   \item{particles0}{Initial particle release table.}
#'   \item{release_sites_m}{Release sites transformed to `crs_m`, with release
#'   coordinates.}
#'   \item{land_m}{Land polygon transformed to `crs_m`, or `NULL` if
#'   `land_poly = NULL`.}
#' }
#'
#' @details
#' Particle movement follows:
#'
#' \deqn{x_{t + 1} = x_t + u_t dt + \epsilon_x}
#'
#' \deqn{y_{t + 1} = y_t + v_t dt + \epsilon_y}
#'
#' where `u_t` and `v_t` are the nearest-neighbour flow velocities and
#' \eqn{\epsilon_x} and \eqn{\epsilon_y} are normally distributed random-walk
#' terms with standard deviation:
#'
#' \deqn{\sqrt{2 K dt}}
#'
#' Particles that intersect `land_poly` after movement are redrawn from the
#' previous position using the same advective and diffusive step. If a particle
#' still intersects land after `max_land_retry` attempts, it is returned to its
#' previous position for that time step.
#'
#' Flow fields are matched exactly by timestamp. For this reason,
#' `release_by = paste(dt, "sec")` is recommended unless the flow field has
#' finer temporal resolution.
#'
#' @examples
#' \dontrun{
#' particle_sim <- simulate_particles(
#'   flow_field = flow_sim$flow_field,
#'   land_poly = land_poly,
#'   release_sites = release_sites,
#'   crs_m = 32755,
#'   dt = 5 * 60,
#'   release_duration_seconds = 60 * 60,
#'   n_particles_per_site_per_release = 100,
#'   K = 0.05,
#'   max_land_retry = 200,
#'   seed = 101,
#'   parallel = TRUE,
#'   workers = 6
#' )
#'
#' particle_sim$particle_tracks_sf
#' particle_sim$final_particles_4326
#' particle_sim$particle_track_summary
#' }
#'
#' @importFrom dplyr mutate select filter arrange group_by ungroup row_number
#'   cur_group_id bind_rows summarise n_distinct slice_max if_else n dense_rank
#' @importFrom tidyr crossing
#' @importFrom stringr str_c str_pad
#' @importFrom sf st_transform st_make_valid st_coordinates st_drop_geometry
#'   st_as_sf st_intersects st_crs
#' @importFrom FNN get.knnx
#' @importFrom stats rnorm
#' @importFrom tibble tibble
#'
#' @export
simulate_particles <- function(
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
) {

  requireNamespace("lubridate")
  requireNamespace("FNN")

  if (!inherits(flow_field, "sf")) {
    stop("`flow_field` must be an sf object.", call. = FALSE)
  }

  if (!inherits(release_sites, "sf")) {
    stop("`release_sites` must be an sf object.", call. = FALSE)
  }

  required_flow_cols <- c("time", "x", "y", "u_ms", "v_ms")
  missing_flow_cols <- setdiff(required_flow_cols, names(flow_field))

  if (length(missing_flow_cols) > 0) {
    stop(
      "`flow_field` is missing required columns: ",
      paste(missing_flow_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(land_poly) && !inherits(land_poly, "sf")) {
    stop("`land_poly` must be NULL or an sf polygon object.", call. = FALSE)
  }

  if (parallel) {
    requireNamespace("future")
    requireNamespace("furrr")
    requireNamespace("purrr")
  }

  set.seed(seed)

  land_m <- NULL

  if (!is.null(land_poly)) {
    land_m <- land_poly |>
      sf::st_transform(crs_m) |>
      sf::st_make_valid()
  }

  release_sites_m <- release_sites |>
    dplyr::mutate(site_id = dplyr::row_number()) |>
    sf::st_transform(crs_m)

  release_xy <- sf::st_coordinates(release_sites_m)

  release_sites_m <- release_sites_m |>
    dplyr::mutate(
      release_x = release_xy[, 1],
      release_y = release_xy[, 2]
    )

  flow_lookup <- flow_field |>
    sf::st_transform(crs_m) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      time = as.POSIXct(.data$time, tz = "UTC")
    ) |>
    dplyr::select(
      .data$time,
      .data$x,
      .data$y,
      .data$u_ms,
      .data$v_ms
    )

  flow_times <- sort(unique(flow_lookup$time))

  if (length(flow_times) == 0) {
    stop("`flow_field` contains no valid flow-field times.", call. = FALSE)
  }

  if (is.null(release_start)) {
    release_start <- min(flow_times, na.rm = TRUE)
  }

  release_start <- as.POSIXct(release_start, tz = "UTC")

  release_times <- seq(
    from = release_start,
    to = release_start + release_duration_seconds - 1,
    by = release_by
  )

  release_times <- as.POSIXct(release_times, origin = "1970-01-01", tz = "UTC")

  nearest_flow <- function(particles_t, flow_t) {

    nn <- FNN::get.knnx(
      data = flow_t |>
        dplyr::select(.data$x, .data$y) |>
        as.matrix(),
      query = particles_t |>
        dplyr::select(.data$x, .data$y) |>
        as.matrix(),
      k = 1
    )

    particles_t |>
      dplyr::mutate(
        u_ms = flow_t$u_ms[nn$nn.index[, 1]],
        v_ms = flow_t$v_ms[nn$nn.index[, 1]]
      )
  }

  check_land_hit <- function(dat, land_m, crs_m) {

    if (is.null(land_m)) {
      return(rep(FALSE, nrow(dat)))
    }

    dat_sf <- dat |>
      sf::st_as_sf(
        coords = c("x", "y"),
        crs = crs_m,
        remove = FALSE
      )

    lengths(sf::st_intersects(dat_sf, land_m)) > 0
  }

  move_particles <- function(
    particles_t,
    flow_lookup,
    land_m,
    tt,
    dt = 5 * 60,
    K = 0.05,
    max_land_retry = 200
  ) {

    tt <- as.POSIXct(tt, origin = "1970-01-01", tz = "UTC")

    flow_t <- flow_lookup |>
      dplyr::filter(.data$time == tt)

    if (nrow(flow_t) == 0) {
      return(particles_t)
    }

    active <- particles_t |>
      dplyr::filter(.data$status == "active")

    inactive <- particles_t |>
      dplyr::filter(.data$status != "active")

    if (nrow(active) == 0) {
      return(particles_t)
    }

    rw_sd <- sqrt(2 * K * dt)

    active_next <- active |>
      nearest_flow(flow_t = flow_t) |>
      dplyr::mutate(
        x_old = .data$x,
        y_old = .data$y,
        x = .data$x + .data$u_ms * dt +
          stats::rnorm(dplyr::n(), mean = 0, sd = rw_sd),
        y = .data$y + .data$v_ms * dt +
          stats::rnorm(dplyr::n(), mean = 0, sd = rw_sd),
        time = tt + dt,
        age_seconds = .data$age_seconds + dt,
        land_retry_n = 0L
      )

    hit_land <- check_land_hit(
      dat = active_next,
      land_m = land_m,
      crs_m = crs_m
    )

    retry_i <- 0L

    while (any(hit_land) && retry_i < max_land_retry) {

      retry_i <- retry_i + 1L
      n_hit <- sum(hit_land)

      active_next$x[hit_land] <- active_next$x_old[hit_land] +
        active_next$u_ms[hit_land] * dt +
        stats::rnorm(n_hit, mean = 0, sd = rw_sd)

      active_next$y[hit_land] <- active_next$y_old[hit_land] +
        active_next$v_ms[hit_land] * dt +
        stats::rnorm(n_hit, mean = 0, sd = rw_sd)

      active_next$land_retry_n[hit_land] <-
        active_next$land_retry_n[hit_land] + 1L

      hit_land <- check_land_hit(
        dat = active_next,
        land_m = land_m,
        crs_m = crs_m
      )
    }

    active_next <- active_next |>
      dplyr::mutate(
        x = dplyr::if_else(hit_land, .data$x_old, .data$x),
        y = dplyr::if_else(hit_land, .data$y_old, .data$y),
        status = "active"
      ) |>
      dplyr::select(
        .data$particle_id,
        .data$particle_uid,
        .data$site_id,
        .data$particle_i,
        .data$release_i,
        .data$release_time,
        .data$time,
        .data$age_seconds,
        .data$x,
        .data$y,
        .data$status,
        .data$land_retry_n
      )

    dplyr::bind_rows(inactive, active_next)
  }

  run_serial <- function(release_sites_m_i, seed_i) {

    set.seed(seed_i)

    particles0 <- release_sites_m_i |>
      sf::st_drop_geometry() |>
      dplyr::select(
        .data$site_id,
        .data$release_x,
        .data$release_y
      ) |>
      tidyr::crossing(
        release_time = release_times,
        particle_i = seq_len(n_particles_per_site_per_release)
      ) |>
      dplyr::group_by(.data$site_id, .data$release_time) |>
      dplyr::mutate(
        release_i = dplyr::cur_group_id()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        particle_uid = stringr::str_c(
          "site", stringr::str_pad(.data$site_id, 2, pad = "0"),
          "_rel", stringr::str_pad(.data$release_i, 3, pad = "0"),
          "_p", stringr::str_pad(.data$particle_i, 4, pad = "0")
        ),
        particle_id = dplyr::row_number(),
        time = .data$release_time,
        age_seconds = 0,
        x = .data$release_x,
        y = .data$release_y,
        status = "active",
        land_retry_n = 0L
      ) |>
      dplyr::select(
        .data$particle_id,
        .data$particle_uid,
        .data$site_id,
        .data$particle_i,
        .data$release_i,
        .data$release_time,
        .data$time,
        .data$age_seconds,
        .data$x,
        .data$y,
        .data$status,
        .data$land_retry_n
      )

    sim_times <- flow_times[
      flow_times >= min(release_times, na.rm = TRUE) &
        flow_times <= max(flow_times, na.rm = TRUE)
    ]

    particles_active <- tibble::tibble()
    particle_tracks <- vector("list", length(sim_times))

    for (i in seq_along(sim_times)) {

      tt <- sim_times[i]

      new_particles <- particles0 |>
        dplyr::filter(.data$release_time == tt)

      particles_active <- dplyr::bind_rows(
        particles_active,
        new_particles
      )

      particles_active <- move_particles(
        particles_t = particles_active,
        flow_lookup = flow_lookup,
        land_m = land_m,
        tt = tt,
        dt = dt,
        K = K,
        max_land_retry = max_land_retry
      )

      particle_tracks[[i]] <- particles_active |>
        dplyr::mutate(
          track_step = i,
          track_time = tt
        )
    }

    list(
      particles0 = particles0,
      particle_tracks = dplyr::bind_rows(particle_tracks)
    )
  }

  if (parallel && nrow(release_sites_m) > 1) {

    if (is.null(workers)) {
      workers <- max(1, parallel::detectCores() - 1)
    }

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    future::plan(future::multisession, workers = workers)

    release_site_list <- split(
      release_sites_m,
      release_sites_m$site_id
    )

    sims <- furrr::future_map2(
      release_site_list,
      seq_along(release_site_list),
      \(release_sites_m_i, i) {
        run_serial(
          release_sites_m_i = release_sites_m_i,
          seed_i = seed + i
        )
      },
      .options = furrr::furrr_options(seed = TRUE)
    )

  } else {

    sims <- list(
      run_serial(
        release_sites_m_i = release_sites_m,
        seed_i = seed
      )
    )
  }

  particles0 <- sims |>
    purrr::map("particles0") |>
    dplyr::bind_rows()

  particle_tracks <- sims |>
    purrr::map("particle_tracks") |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      particle_uid = as.character(.data$particle_uid),
      particle_id = dplyr::dense_rank(.data$particle_uid)
    ) |>
    dplyr::arrange(
      .data$particle_id,
      .data$time,
      .data$track_step
    )

  particles0 <- particles0 |>
    dplyr::mutate(
      particle_uid = as.character(.data$particle_uid),
      particle_id = dplyr::dense_rank(.data$particle_uid)
    )

  particle_tracks_sf <- particle_tracks |>
    sf::st_as_sf(
      coords = c("x", "y"),
      crs = crs_m,
      remove = FALSE
    )

  final_particles <- particle_tracks_sf |>
    dplyr::group_by(.data$particle_id, .data$particle_uid) |>
    dplyr::slice_max(.data$time, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  particle_track_summary <- particle_tracks |>
    dplyr::summarise(
      n_particles = dplyr::n_distinct(.data$particle_id),
      n_particle_uids = dplyr::n_distinct(.data$particle_uid),
      n_track_rows = dplyr::n(),
      mean_land_retry_n = mean(.data$land_retry_n, na.rm = TRUE),
      max_land_retry_n = max(.data$land_retry_n, na.rm = TRUE),
      prop_retried_land = mean(.data$land_retry_n > 0, na.rm = TRUE),
      parallel = parallel,
      workers = ifelse(is.null(workers), 1L, workers)
    )

  list(
    particle_tracks = particle_tracks,
    particle_tracks_sf = particle_tracks_sf,
    particle_tracks_4326 = sf::st_transform(particle_tracks_sf, 4326),
    final_particles = final_particles,
    final_particles_4326 = sf::st_transform(final_particles, 4326),
    particle_track_summary = particle_track_summary,
    particles0 = particles0,
    release_sites_m = release_sites_m,
    land_m = land_m
  )
}
