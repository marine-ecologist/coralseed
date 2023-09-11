

library(coralseed)
library(tidyverse)
library(doParallel)
library(foreach)
library(sf)
library(units)
library(tictoc)
library(rlang)

library(coralseed)
seascape <- coralseed::seascape_probability(reefoutline=reef_map, habitat=benthic_map)

sim1_100k <- st_read("/Users/rof011/Library/CloudStorage/OneDrive-CSIRO/Data - SeaSims/conniemodels/run_day_12036_lizard_del_14_1512_sim1_100K_10.json")



### 1) CONNIE variance


nsims=1
ncores <- detectCores()
cl <- makeCluster(ncores-1)
registerDoParallel(cl)

devtools::install("/Users/rof011/coralseed")

dispersal_variance_output <- foreach(i=1:nsims, .combine='rbind', .export=c("sim1_100k", "seascape"), .packages=c("tidyr", "forcats", "coralseed")) %dopar% {


  sim_random <- sim1_100k |> filter(id %in% sample(0:99999, 1000))


  output_single <- seed_particles(input = sim_random, seascape = seascape, set.centre = TRUE, silent = TRUE, competency.function = "exponential",  set.seed=123, simulate.mortality = "typeI", simulate.mortality.n = 0)

  t1 <- particle_distances(output_single, 60, type="sf") |> as.data.frame() |> select(-geometry) |> mutate(dispersaltime="T1")
  t2 <- particle_distances(output_single, 120, type="sf") |> as.data.frame() |> select(-geometry) |> mutate(dispersaltime="T2")
  t4 <- particle_distances(output_single, 240, type="sf") |> as.data.frame() |> select(-geometry) |> mutate(dispersaltime="T4")
  t6 <- particle_distances(output_single, 360, type="sf") |> as.data.frame() |> select(-geometry) |> mutate(dispersaltime="T6")
  t8 <- particle_distances(output_single, 480, type="sf") |> as.data.frame() |> select(-geometry) |> mutate(dispersaltime="T8")
  t10 <- particle_distances(output_single, 600, type="sf") |> as.data.frame() |> select(-geometry) |> mutate(dispersaltime="T10")

  dispersal_variance <- rbind(t2, t4, t6, t8, t10) #|> mutate(sim=as.factor(i))

  dispersal_variance

}

stopCluster(cl)



library(ggridges)

ggplot() + theme_bw() + facet_wrap(~dispersaltime, scales="free") + ggtitle("high retention - mermaid bay") +
  geom_density_ridges(data=Mermaid_output, aes(distance, sim, fill=sim), alpha=0.6) +
  scale_fill_viridis_d(direction=-1)

mermaid_distances
ggsave(mermaid_distances, filename="mermaid_distances.pdf")
