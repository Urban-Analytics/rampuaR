#' Devon PHE cases 
#'
#' A data set containing smoothed PHE cases for Devon. 
#' Case numbers have been multiplied by 20 to get the desired 2% 
#' prevalence of COVID for this time-period
#'
#' @format A vector of integers, 103 in length:
#' @source \describe{PHE data}
"gam_cases"

#' High Risk MSOAS 
#'
#' MSOAs in Devon and their risk level for COVID in terms of population density,
#' transport connectivity and wealth.
#'
#' @format A data frame with 107 rows and 3 variables:
#' \describe{
#'   \item{area}{Name of MSOA}
#'   \item{risk}{Risk tertile, Low, Medium or High}
#'   ...
#' }
#' @source \describe{Calculated by Fiona Spooner # will put code on github}
"msoas"

