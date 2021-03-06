% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covid_status_functions.R
\name{infection_prob}
\alias{infection_prob}
\title{Calculating probabilities of becoming a COVID case}
\usage{
infection_prob(df, dt = 1)
}
\arguments{
\item{df}{The input list - the output from the create_input function}

\item{dt}{Time unit}
}
\value{
An updated version of the input list with the probabilties updated
}
\description{
Calculating probabilities of becoming a COVID case based on each individuals
'current_risk'
}
