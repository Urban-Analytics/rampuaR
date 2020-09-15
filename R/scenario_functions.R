# Functions for running scenarios

# increase probability of getting covid
# for people in local outreak msoa
#' @param df data frame with individuals
#' @param msoa_infect msoa where the local outbreak occurs
#' @param number_people number of people at local outbreak event
#' @param risk_prob value to change probability of infection to for people involved in local outbreak
#' @return updated dataframe of individuals with increased probability of getting infected for individuals at local outbreak
#' @export
local_outbreak <- function(df, 
                           msoa_infect,
                           number_people=100,
                           risk_prob=0.75){
  
  df$probability[df$area==msoa_infect][1:number_people] <- risk_prob
  
  return(df)
}
