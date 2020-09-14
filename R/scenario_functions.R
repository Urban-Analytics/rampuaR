# Functions for running scenarios

# increase probability of getting covid
# for people in local outreak msoa
local_outbreak <- function(df, 
                           msoa_infect,
                           number_people=100,
                           risk_prob=0.75){
  
  df[df$area==msoa_infect,][1:number_people,'probability'] <- risk_prob
  
  return(df)
}
