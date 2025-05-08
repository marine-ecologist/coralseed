
# coralseed

<!-- badges: start -->
<!-- badges: end -->

 
`coralseed` is a spatially explicit probabilistic model aims to quantify the spatial footprint of coral larval re-seeding from restoration progjects. The model is based on input from high-resolution oceanographic models ([CONNIE](https://connie.csiro.au)) that tracking individual particles through space and time. By parameterising larval behaviour (competency, habitat specific substrate settlement preferences, swimming behaviour) individual-based probability of settlement can be modelled for 10^5^ - 10^7^ larvae. Individual-based settlement probability an dispersal paths then overlaid onto high-resolution habitat maps ([Allen Coral Atlas](https://www.allencoralatlas.org)) to form a spatially-explicit model of settlement following release of coral larvae.

`coralseed` aims to provide insight into key knowledge gaps in larval restoration: 1) Where do larvae settle following release? 2) What is the spatial footprint of larval reseeding projects? 3) what are the likely densities of settled corals? 4) What densities of adult corals (\~10cm size) are produced from reseeding and where are they located? By varying initial model parameters (e.g. tidal currents, time of release, larval competency, larval densities, location of release sites), a simulation modelling approach can be used to quantify the likely spatial footprint and expected settlement densities, allowing for optimisation and upscaling of larval reseeding programs on the Great Barrier Reef and elsewhere.

## Installation

`coralseed` can be installed on github 

``` r
#install.packages("remotes")
install_github("marine-ecologist/coralseed")

```
Below is a base parameterisation of coralseed across a 6.95hr time period (limit_time) with an exponential time-to-settlement model parameterisation (competency.function) with a type I mortality curve (simulate.mortality) applied to 10% of the population over a 24 hr period (simulate.mortality.n) with an additive settlement function (probability).

```r 
library(coralseed)
library(ggplot2)
library(tidyverse)
library(sf)

## 1. simulate settlement probabilities from habitat maps
# `seascape_probability` takes coral atlas inputs and a data.frame of settlement
# probability (mean, se) to simulate probability of settlement in habitats surrounding the release area

seascape <- seascape_probability(reefoutline=reef_map, habitat=benthic_map)

## 2. seed particles from dispersal model and simulate competency
# `seed_particles` outputs summary statistics and a four panel diagnostic plot

particles <- seed_particles(example="mermaid", seascape=seascape,
                            limit_time=6.95, competency.function = "exponential", probability="additive",
                            simulate.mortality = "typeI",  simulate.mortality.n = 0.1, 
                            silent=FALSE, return.plot=TRUE)

## 3. simulate settlement of particles
# `settle_particles` then applies probability across the seascape to simulate spatially
# explicit patterns of settlement across the seascape:

settlers <-  settle_particles(particles, probability="additive")# %>% with(points)

## 4. Map combined coralseed outputs
# `map_coralseed` visualises all outputs from the above three models

map_coralseed(seed_particles=particles, settle_particles=settlers, seascape_probability=seascape, restoration.plot=c(100,100))

```
