% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seed_particles.R
\name{seed_particles}
\alias{seed_particles}
\title{Seed particles}
\usage{
seed_particles(
  input = NULL,
  zarr = FALSE,
  seascape = NULL,
  subsample = NULL,
  simulate.mortality = "none",
  simulate.mortality.n = 0.1,
  brmsfit = infamis_tiles_exp,
  set_b_Intercept = NULL,
  limit.time = NA,
  set.centre = TRUE,
  seed.value = NULL,
  crs = 32755,
  silent = TRUE,
  return.summary = FALSE,
  return.plot = FALSE,
  save.plot = FALSE,
  plot.width = 12,
  plot.height = 7.5,
  ...
)
}
\arguments{
\item{input}{input}

\item{zarr}{whether the input is a zarr folder}

\item{seascape}{shp file inputs from seascape_probability()}

\item{subsample}{subsample to n samples}

\item{simulate.mortality}{set mortality type via simulate_mortality() one of "typeI","typeII", "typeIII" (defaults to "none")}

\item{simulate.mortality.n}{set proportion of corals to kill over a 24hr period, where 0 is none, 1 is 100 (defaults to 0.1 or 10\%)}

\item{brmsfit}{a brmsfit model for predicting competency}

\item{set_b_Intercept}{manually set intercept (for development)}

\item{limit.time}{limit the time series (e.g. 720 mins)}

\item{set.centre}{reset CONNIE input to have a central t0 point (defaults to TRUE)}

\item{seed.value}{set seed for consistent results (defaults to NULL)}

\item{crs}{coordinate reference system, default EPSG:32755}

\item{silent}{silence printing results (defaults to TRUE)}

\item{return.summary}{return summary table}

\item{return.plot}{return plot object}

\item{save.plot}{path to save plot image}

\item{plot.width}{width of saved plot}

\item{plot.height}{height of saved plot}

\item{...}{passes functions}
}
\description{
Function to seed particles
}
\details{
seed_particles() uses predict_competency(), simulate_mortality()

seed_particles(input = "Users/rof011/coralseed/data-raw/run_day_11656_lizard_fcst_15_2611_26.json",
seascape = seascape,
brmsfit=infamis_tiles_exp,
simulate.mortality = "typeI",
simulate.mortality.n = 0.1)
}
