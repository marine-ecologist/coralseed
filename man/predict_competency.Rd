% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_competency.R
\name{predict_competency}
\alias{predict_competency}
\title{Predict_competency}
\usage{
predict_competency(
  input,
  n_particles = 100,
  seed.value = NULL,
  return.plot = FALSE
)
}
\arguments{
\item{input}{brmsfit of either 'lognormal', 'exponential', 'weibull' functions}

\item{n_particles}{number of sims for randomised datasets (see plot for traces)}

\item{seed.value}{set seed value, defaults to NULL}

\item{return.plot}{return output}

\item{...}{passes functions}
}
\description{
Function to generate probability distribution from Allen Coral Atlas input files
}
\details{
predict_competency_time(infamis_tiles_exp, n_particles=100)
predict_competency_time(infamis_tiles_log, n_particles=100)
predict_competency(infamis_tiles_weibull, n_particles=100)
}
