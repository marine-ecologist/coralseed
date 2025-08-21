

library(tmap)
library(tmap.mapgl)
tmap_mode("maplibre")
tmap_mode("mapbox")

parts <- purrr::transpose(results_list[1:1])
lizard_particles_points       <- parts$lizard_particles_points |> bind_rows()
lizard_particles_linestrings  <- parts$lizard_particles_linestrings |> bind_rows()
lizard_settlers_points        <- parts$lizard_settlers_points |> bind_rows()
lizard_settlers_linestrings   <- parts$lizard_settlers_linestrings |> bind_rows()
lizard_particles_linestrings_competency <- parts$lizard_particles_linestrings_competency |> bind_rows()
lizard_particles_linestrings_probability <- parts$lizard_particles_linestrings_competency |> bind_rows()

lizard_particles_linestrings_competency <- lizard_particles_linestrings_competency |>
  mutate(dispersaltime = (time - min(lizard_particles_linestrings$time))) |>
  mutate(dispersaltime = round(as.numeric(dispersaltime, units = "secs") / 3600, 1))

lizard_particles_linestrings <- lizard_particles_linestrings |>
  mutate(dispersaltime = (time - min(lizard_particles_linestrings$time))) |>
  mutate(dispersaltime = round(as.numeric(dispersaltime, units = "secs") / 3600, 1))


lengthlist <- lizard_particles_linestrings_competency %>%
  mutate(length=st_length(.)) |> group_by(id) |> summarise(length=sum(length)) |>
  slice_max(n=1, order_by=length)

lengthlist_tail <- lizard_particles_linestrings_competency %>%
  mutate(length=st_length(.)) |> group_by(id) |> summarise(length=sum(length)) |>
  slice_min(n=100, order_by=length)



lizard_bbox <- st_bbox(c(xmin=1628800, ymin=8347640, xmax=1633660, ymax=8354525), crs=20353)
lizard_bbox2 <- st_bbox(c(xmin=1630500, ymin=8352000, xmax=1633660, ymax=8354525), crs=20353)
lizard_bbox3 <- st_bbox(c(xmin=1631000, ymin=8354000, xmax=1633000, ymax=8354525), crs=20353)


competent_larvae <- lizard_particles_linestrings_competency |>
  group_by(id) %>%
  filter(any(competency == "competent", na.rm = TRUE)) %>%
  ungroup() |>
  group_by(id) |>
  st_cast("MULTILINESTRING")

select_larvae <- sample(competent_larvae$id, 50)

competency <- competent_larvae |> dplyr::filter(id %in% select_larvae) |> st_transform(20353) |>
  group_by(id, competency) |> st_cast("MULTILINESTRING") |> st_simplify(dTolerance = 0.001, preserveTopology = TRUE) |>
  filter(lubridate::minute(time) %% 2 == 0)

dispersaltime <- lizard_particles_linestrings  |>
  filter(id %in% unique(competency$id)) |> filter(id %in% select_larvae) |>
  st_simplify(dTolerance = 0.001, preserveTopology = TRUE) |>
  filter(lubridate::minute(time) %% 2 == 0)

particles <- lizard_particles_linestrings |>
  filter(id %in% unique(competency$id)) |> filter(id %in% select_larvae) |> st_transform(20353) |>
  st_simplify(dTolerance = 0.001, preserveTopology = TRUE) |>
  filter(lubridate::minute(time) %% 2 == 0)

particlepoints <- lizard_particles_points |> dplyr::filter(id %in% select_larvae) |> st_transform(20353) |>
  filter(lubridate::minute(time) %% 2 == 0)

  #group_by(id, competency) |> st_cast("MULTILINESTRING") |> st_simplify(dTolerance = 0.001, preserveTopology = TRUE)

settlerpoints <- lizard_settlers_points #|> dplyr::filter(id %in% select_larvae) |> st_transform(20353)

#


#### make cont particles


Ldt <- as.data.table(particlepoints)

cumulative_linestring_from_points <- function(DT, id_col = "id", time_col = "time") {
  # Split row indices per id
  idx_by_id <- split(seq_len(nrow(DT)), DT[[id_col]])

  # Build cumulative LINESTRING per id
  out <- vector("list", length(idx_by_id))
  k <- 0L
  for (ix in idx_by_id) {
    # coords for this id in time order
    xv <- DT[ ix, .x ]
    yv <- DT[ ix, .y ]
    tv <- DT[ ix, get(time_col) ]
    n  <- length(ix)

    # For each time step k>=2, create a linestring from 1..k
    geoms <- vector("list", max(0L, n - 1L))
    times <- vector("list", max(0L, n - 1L))
    if (n > 1L) {
      for (i in 2:n) {
        geoms[[i-1]] <- st_linestring(cbind(xv[1:i], yv[1:i]))
        times[[i-1]] <- tv[i]  # “current” time stamp
      }
    }

    # Assemble sf for this id (skip empty)
    if (length(geoms)) {
      k <- k + 1L
      out[[k]] <- st_sf(
        id   = DT[[id_col]][ix[2:n]],
        time = as.POSIXct(unlist(times), origin = "1970-01-01"),
        geometry = st_sfc(geoms, crs = crs0)
      )
    }
  }
  # Bind results
  if (k == 0L) {
    return(st_sf(
      id = DT[[id_col]][0],
      time = as.POSIXct(character()),
      geometry = st_sfc(crs = crs0)
    ))
  }
  do.call(rbind, out)
}

# 1) prepare DT with coords
xy  <- sf::st_coordinates(particlepoints)[,1:2, drop = FALSE]
Ldt <- data.table::as.data.table(sf::st_drop_geometry(particlepoints))
Ldt[, `.x` := xy[,1]]
Ldt[, `.y` := xy[,2]]
data.table::setorderv(Ldt, c("id","time"))

# 2) define crs0 in the caller (function uses it)
crs0 <- sf::st_crs(particlepoints)

# 3) now this will work
cumulative_lines <- cumulative_linestring_from_points(Ldt) |>
  mutate(dispersaltime = (time - min(Ldt$time))) |>
  mutate(dispersaltime = round(as.numeric(dispersaltime, units = "secs") / 60, 0)-2) |>
  st_as_sf(crs = st_crs(particlepoints))


tmap_mode("plot")

particlepoints <- particlepoints |>
  filter(dispersaltime < 719)

anim_particles <-

  tm_shape(particlepoints |>
             arrange(id, dispersaltime) |>
             mutate(dispersaltime=as.factor(dispersaltime)), bbox=lizard_bbox2) +
  tm_dots(lwd=0.5,
          shape=21,
          fill="dispersaltime",
          fill.legend = tm_legend_hide(),
          fill.scale = tm_scale_continuous(values="-brewer.rd_yl_bu", limits=c(0,720))) +
  tmap_options(facet.max = 720) +
  tm_animate_fast(frame="dispersaltime", fps = 20) +
  tm_layout(
    bg.color = NA,        # transparent background
    frame = FALSE,        # remove bounding box
    outer.bg.color = NA   # transparent outside the map
  )

#tmap_animation(anim_particles, "/Users/rof011/coralseed/tma1.gif")



anim_particles2 <-
  tm_basemap("Esri.WorldImagery", alpha=1) +
  tm_shape(cumulative_lines |>
             arrange(id, dispersaltime) |>
             mutate(dispersaltime=as.factor(dispersaltime)),
           bbox=lizard_bbox2) +
  tm_lines(lwd=0.5,
           col="dispersaltime",
           col.legend = tm_legend_hide(),
           col.scale = tm_scale_continuous(values="white")) +
  tmap_options(facet.max = 720) +
  tm_animate_fast(frame="dispersaltime", fps = 20) +
  tm_layout(
    bg.color = NA,        # transparent background
    frame = FALSE,        # remove bounding box
    outer.bg.color = NA   # transparent outside the map
  )




#tmap_animation(anim_particles2, "/Users/rof011/coralseed/tma2.gif")
tic()
tmap_animation(anim_particles+anim_particles2, "/Users/rof011/coralseed/tma3.gif", width=1200, height=1200)
toc()

