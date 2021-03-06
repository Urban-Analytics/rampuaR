% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covid_status_functions.R
\name{exp_cdf}
\alias{exp_cdf}
\title{Exponential distribution cdf, used to integrate intensity (0, inf) over 
time to a probability (0, 1)}
\usage{
exp_cdf(intensity, dt)
}
\arguments{
\item{intensity}{current_risk from spatial interaction model}

\item{dt}{timestep to integrate}
}
\value{
probability of infection
}
\description{
Exponential distribution cdf, used to integrate intensity (0, inf) over 
time to a probability (0, 1)
}
