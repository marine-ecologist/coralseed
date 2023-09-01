
# coralseed

<!-- badges: start -->
<!-- badges: end -->

![coralseedpalau](man/figures/coralseedpalau.jpg)
 
`coralseed` is a spatially explicit probabilistic model aims to quantify the spatial footprint of coral larval re-seeding from restoration progjects. The model is based on input from high-resolution oceanographic models ([CONNIE](https://connie.csiro.au)) that tracking individual particles through space and time. By parameterising larval behaviour (competency, habitat specific substrate settlement preferences, swimming behaviour) individual-based probability of settlement can be modelled for 10^5^ - 10^7^ larvae. Individual-based settlement probability an dispersal paths then overlaid onto high-resolution habitat maps ([Allen Coral Atlas](https://www.allencoralatlas.org)) to form a spatially-explicit model of settlement following release of coral larvae.

![coralseed](man/figures/img1.png)

`coralseed` aims to provide insight into key knowledge gaps in larval restoration: 1) Where do larvae settle following release? 2) What is the spatial footprint of larval reseeding projects? 3) what are the likely densities of settled corals? 4) What densities of adult corals (\~10cm size) are produced from reseeding and where are they located? By varying initial model parameters (e.g. tidal currents, time of release, larval competency, larval densities, location of release sites), a simulation modelling approach can be used to quantify the likely spatial footprint and expected settlement densities, allowing for optimisation and upscaling of larval reseeding programs on the Great Barrier Reef and elsewhere.

![coralseed2](man/figures/img2.png)


## Installation

`coralseed` can be installed on github 

``` r
#install.packages("remotes")
install_github("marine-ecologist/coralseed")

```

## Example

`coralseed` is pre-loaded with five point-source dispersal scenarios from Lizard Island (northern Great Barrier Reef) in December 2022. 1000 simulated particles were released from a 25m square at 3m depth 1.5 hours before high tide in each simulation (14:00hrs).

| **input**                              | **Location**  | **Release type**   |
|------------------------------------|------------------|------------------|
| `PalfreyN_PointSource_ForeReefEx_01`   | Palfrey North | Forereef           |
| `Mermaid_PointSource_Bay_01`           | Mermaid Bay   | Bay                |
| `ClamGarden_PointSource_OpenLagoon_01` | Clam Garden   | Open Lagoon        |
| `SpHub_PointSource_SELaggon_01`        | Spawning hub  | SE Lagoon          |
| `WatsonN_PointSource_ForeReefSh_01`    | Watsons North | Sheltered Forereef |

Below is a base paramaterisation of `coralseed` across a 6.95hr time period (`limit_time`) with an exponential time-to-settlement model parameterisation (`competency.function`) with a type I mortality curve (`simulate.mortality`) applied to 10% of the population over a 24 hr period (`simulate.mortality.n`) with an additive settlement function (`probability`).


```r 
#devtools::install_github("marine-ecologist/coralseed",  lib = "/Users/rof011/coralseed")
devtools::document("/Users/rof011/coralseed")

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

particles <- seed_particles(input="Mermaid_PointSource_Bay_01", seascape=seascape,
                            limit_time=6.95, competency.function = "exponential", 
                            limit.time = 720, simulate.mortality = "typeI", 
                            simulate.mortality.n = 0.1, probability="additive",
                            return.plot=TRUE)

## 3. simulate settlement of particles
# `settle_particles` then applies probability across the seascape to simulate spatially
# explicit patterns of settlement across the seascape:

settlers <-  settle_particles(particles, probability="additive")# %>% with(points)

## 4. Map combined coralseed outputs
# `map_coralseed` visualises all outputs from the above three models

map_coralseed(seed_particles=particles, settle_particles=settlers, seascape_probability=seascape, restoration.plot=c(100,100))

```
