# Functions for running scenarios

#' increase probability of getting covid
#' for people in local outreak msoa
#' @param df data frame with individuals
#' @param msoa_infect msoa where the local outbreak occurs
#' @param number_people number of people at local outbreak event
#' @param risk_prob value to change probability of infection to for people involved in local outbreak
#' @return updated dataframe of individuals with increased probability of getting infected for individuals at local outbreak
#' @export
local_outbreak <- function(df, 
                           msoa_infect,
                           number_people=100,
                           risk_prob=1.0){
  
  if (msoa_infect == "none") {
    num <- length(df$probability)
    ran_samp <- sample(1:num, number_people)
    print(paste(number_people, "people exposed"))
    df$probability[ran_samp] <- risk_prob
  } else {
    num <- length(df$probability[df$area==msoa_infect])
    print(paste("people in msoa",num))
    if (num<number_people){
      number_people <- num
    }
    ran_samp <- sample(1:num, number_people)
    print(paste(number_people, "people exposed"))
    
    df$probability[df$area==msoa_infect][ran_samp] <- risk_prob
  }

  
  return(df)
}




