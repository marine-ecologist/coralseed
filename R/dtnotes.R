library(data.table)

load_particles <- data_sources[["mermaid"]] %>%
  sf::st_zm(drop = TRUE, what = "ZM") %>%
  sf::st_transform(20353) %>%
  dplyr::mutate(time = time + lubridate::hours(14))

t0 <- min(load_particles$time)
tmax <- max(load_particles$time)
n_id <- length(unique(load_particles$id))

load_particles <- as.data.table(load_particles)
load_particles[, dispersaltime := as.numeric(time - min(t0)) / 60]

# if (!is.na(limit.time)) {
#   load_particles <- load_particles[dispersaltime <= limit.time * 60]
# }



load_particles[, c("X", "Y") := .(sf::st_coordinates(geometry)[, 1], sf::st_coordinates(geometry)[, 2])]

load_particles_t0 <- load_particles[time == min(t0), .(X = mean(X), Y = mean(Y))]
load_particles_t0 <- load_particles_t0[rep(1:.N, each = n_id)]
load_particles_t0[, `:=`(id = .I - 1, time = min(t0), dispersaltime = 0)]

