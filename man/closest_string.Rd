% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/google_mobility_functions.R
\name{closest_string}
\alias{closest_string}
\title{Matching LAD/County to Google Mobility Region}
\usage{
closest_string(lad_string, county_string, strings_to_match)
}
\arguments{
\item{lad_string}{The LAD to try and match to Google Mobility region}

\item{county_string}{The county to try and match to Google Mobility region}

\item{strings_to_match}{Vector of sub-regions from the Google Mobility Data}
}
\value{
The name of the Google Mobility sub-region that most closely
 matches the LAD/county name
}
\description{
Matching the name of the LAD/County to the closest matching Google Mobility 
Region
}
