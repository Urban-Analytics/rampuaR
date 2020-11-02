# status column is coded
# 0 = susceptible
# 1 = exposed
# 2 = presymp
# 3 = symp
# 4 = asymp
# 5 = recovered
# 6 = dead

#' Formatting data for infection model
#'
#' Formatting the output of the spatial interaction model for use in the
#' infection model and selecting which variables should be included.
#' 
#' BMI data recoded as:
#' "Not applicable" =  NA
#' "Underweight: less than 18.5" = 0
#' "Normal: 18.5 to less than 25" = 1
#' "Overweight: 25 to less than 30" = 2
#'  "Obese I: 30 to less than 35" = 3
#' "Obese II: 35 to less than 40" = 4
#' "Obese III: 40 or more" = 5
#'
#' @param micro_sim_pop The output of the spatial interaction model
#' @param vars Variables to be kept for use in the infection model
#' @return A list of data to be used in the infection model
#' @export
create_input <- function(micro_sim_pop,
                         vars = NULL){

    if (!all(vars %in% colnames(micro_sim_pop))) {
      print(paste0(vars[!vars %in% colnames(micro_sim_pop)], " not in population column names"))
    }

    var_list <- list()
    for (i in 1:length(vars)) {
      var_list[[i]] <- micro_sim_pop[, vars[i]]
    }

    names(var_list) <- vars

    constant_list <- list(
      beta0 = rep(0, nrow(micro_sim_pop)),
      betaxs = rep(0, nrow(micro_sim_pop)),
      hid_status = rep(0, nrow(micro_sim_pop)),
      exposed_days = micro_sim_pop$exposed_days,
      presymp_days = micro_sim_pop$presymp_days,
      symp_days = micro_sim_pop$symp_days,
      probability = rep(0, nrow(micro_sim_pop)),
      status = as.integer(micro_sim_pop$disease_status),
      new_status = as.integer(micro_sim_pop$disease_status),
      age = as.integer(micro_sim_pop$age),
      mortality_risk = rep(0, nrow(micro_sim_pop)),
      sympt_risk = rep(0, nrow(micro_sim_pop))
    )
    
    df <- c(var_list, constant_list)

    return(df)
  }

#' Calculating mortality rate based on age and health risks
#'
#' @param df The input list - the output from the create_input function
#' @param obesity_40 The obesity mortality risk multiplier for BMI > 40
#' @param obesity_35 The obesity mortality risk multiplier for 35 < BMI < 40 
#' @param obesity_30 The obesity mortality risk multiplier for 30 < BMI < 35
#' @param overweight The obesity mortality risk multiplier for 25 < BMI < 30
#' @param cvd The cardiovascular disease mortality risk multiplier
#' @param diabetes The disease mortality risk multiplier
#' @param bloodpressure The bloodpressure/hypertension mortality risk multiplier
#' @return A list of data to be used in the infection model - 
#' with updated mortality risk
#' @importFrom dplyr case_when
#' @export
#' 
mortality_risk <- function(df, 
                           obesity_40 = 1,
                           obesity_35 = 1,
                           obesity_30 = 1,
                           overweight = 1,
                           cvd = NULL,
                           diabetes = NULL,
                           bloodpressure = NULL){

  df$mortality_risk <- case_when(df$age >= 0 & df$age <= 4 ~ 0,
                                 df$age >= 5 & df$age <= 9 ~ 0.0001,
                                 df$age >= 10 & df$age <= 14 ~ 0.0001,
                                 df$age >= 15 & df$age <= 19 ~ 0.0002,
                                 df$age >= 20 & df$age <= 24 ~ 0.0003,
                                 df$age >= 25 & df$age <= 29 ~ 0.0004,
                                 df$age >= 30 & df$age <= 34 ~ 0.0006,
                                 df$age >= 35 & df$age <= 39 ~ 0.0010,
                                 df$age >= 40 & df$age <= 44 ~  0.0016,
                                 df$age >= 45 & df$age <= 49 ~  0.0024,
                                 df$age >= 50 & df$age <= 54 ~  0.0038,
                                 df$age >= 55 & df$age <= 59 ~  0.0060,
                                 df$age >= 60 & df$age <= 64 ~  0.0094,
                                 df$age >= 65 & df$age <= 69 ~  0.0147,
                                 df$age >= 70 & df$age <= 74 ~  0.0231,
                                 df$age >= 75 & df$age <= 79 ~  0.0361,
                                 df$age >= 80 & df$age <= 84 ~  0.0566,
                                 df$age >= 85 & df$age <= 89 ~  0.0866,
                                 df$age >= 90 & df$age <= 120 ~ 0.1737)
  
  df$mortality_risk <- case_when(df$BMIvg6 == "Obese III: 40 or more" ~ df$mortality_risk * obesity_40,
                                 df$BMIvg6 == "Obese II: 35 to less than 40" ~ df$mortality_risk * obesity_35,
                                 df$BMIvg6 == "Obese I: 30 to less than 35" ~ df$mortality_risk * obesity_30,
                                 df$BMIvg6 == "Overweight: 25 to less than 30" ~ df$mortality_risk * overweight,
                                 is.na(df$BMIvg6) | df$BMIvg6 == "Not applicable" | df$BMIvg6 == "Normal: 18.5 to less than 25" | df$BMIvg6 == "Underweight: less than 18.5" ~ df$mortality_risk)
  
 
  if(!is.null(cvd)){
    df$mortality_risk <- case_when(df$cvd == 1 ~ df$mortality_risk * cvd,
                                   df$cvd == 0 ~ df$mortality_risk,
                                   is.na(df$cvd) ~ df$mortality_risk)
  }
 
  if(!is.null(diabetes)){
    df$mortality_risk <- case_when(df$diabetes == 1 ~ df$mortality_risk * diabetes,
                                   df$diabetes == 0 ~ df$mortality_risk,
                                   is.na(df$diabetes) ~ df$mortality_risk)
  }
  
  if(!is.null(bloodpressure)){
    df$mortality_risk <- case_when(df$bloodpressure == 1 ~ df$mortality_risk * bloodpressure,
                                   df$bloodpressure == 0 ~ df$mortality_risk,
                                   is.na(df$bloodpressure) ~ df$mortality_risk)
  }
 
  
  return(df)
}

#' Function to calculate risk of an individual being symptomatic based on age.
#' 
#' @param age A number
#' 
#' @return Estimated risk of being symptomatic
#' @export
age_symp_risk <- function(age){
  
  age_prob <- diff(approx(y = c(0.21, 0.69), x = c(19, 70), xout = 19:70)$y)[1] ##https://www.nature.com/articles/s41591-020-0962-9
  #figure 3a
  
  if (age < 20){
    sympt_risk <- 0.21
  }
  
  if(age >=20 & age<= 69){
    sympt_risk <- 0.21 + age_prob*(age-19)
  }
  
  if(age > 69){
    sympt_risk <- 0.69
  }
  return(sympt_risk)  
}

#' Calculating risk of being symptomatic mortality rate based on age 
#' and health risks
#'
#' @param df The input list - the output from the create_input function
#' @param overweight_sympt_mplier The obesity risk multiplier for BMI > 40
#' @param cvd The cardiovascular disease mortality risk multiplier
#' @param diabetes The disease mortality risk multiplier
#' @param bloodpressure The bloodpressure/hypertension mortality risk multiplier
#' @return A list of data to be used in the infection model - 
#' with updated mortality risk
#' @importFrom dplyr case_when
#' @export
#' 
sympt_risk <- function(df,
                       overweight_sympt_mplier = 1.46,
                           cvd = NULL,
                           diabetes = NULL,
                           bloodpressure = NULL){

  df$sympt_risk <- sapply(df$age, age_symp_risk)
  
  
  df$sympt_risk <- case_when(df$BMIvg6 %in% c("Obese III: 40 or more","Obese II: 35 to less than 40","Obese I: 30 to less than 35") ~ df$sympt_risk * overweight_sympt_mplier,
                                 is.na(df$BMIvg6) | df$BMIvg6 == "Not applicable" | df$BMIvg6 == "Normal: 18.5 to less than 25" | df$BMIvg6 == "Underweight: less than 18.5"| df$BMIvg6 == "Overweight: 25 to less than 30"  ~ df$sympt_risk)

  if(!is.null(cvd)){
    df$sympt_risk <- case_when(df$cvd == 1 ~ df$sympt_risk * cvd,
                                   df$cvd == 0 ~ df$sympt_risk,
                                   is.na(df$cvd) ~ df$sympt_risk)
  }
  
  if(!is.null(diabetes)){
    df$sympt_risk <- case_when(df$diabetes == 1 ~ df$sympt_risk * diabetes,
                                   df$diabetes == 0 ~ df$sympt_risk,
                                   is.na(df$diabetes) ~ df$sympt_risk)
  }
  
  if(!is.null(bloodpressure)){
    df$sympt_risk <- case_when(df$bloodpressure == 1 ~ df$sympt_risk * bloodpressure,
                                   df$bloodpressure == 0 ~ df$sympt_risk,
                                   is.na(df$bloodpressure) ~ df$sympt_risk)
  }

  
  df$sympt_risk[df$sympt_risk > 1] <- 1
  
  return(df)
}



#' Summing betas for use in COVID probability calculation
#'
#' Calculating probabilities of becoming a COVID case based on each individuals
#' 'current_risk'
#'
#' @param df The input list - the output from the create_input function
#' @param betas List of betas associated with variables to be used in
#' calculating probability of becoming a COVID case
#' @param risk_cap_val The value at which current_risk will be capped
#' @return the sum of betas
#' @export
sum_betas <- function(df, 
                      betas, 
                      risk_cap_val=NA){
  
  if(!is.na(risk_cap_val)){
    print(paste0(sum(df$current_risk > 5), " individuals with risk above  ", risk_cap_val))
    df$current_risk[df$current_risk>risk_cap_val] <- risk_cap_val
  }
  
  beta_names <- names(betas)
  
  if (all(!beta_names %in% names(df))) {
    print(paste0(
      beta_names[!beta_names %in% names(df)],
      " missing from df. They are not included in probabilities."
    ))
  }
  
  beta_names <- beta_names[beta_names %in% names(df)]
  beta_out_sums <- df[[beta_names]] * betas[[beta_names]]
  df$betaxs <- beta_out_sums
  
  return(df)
}

#' Function to map susceptibility
#' @param x factors that might affect susceptibility e.g smoking
#' @return susceptibility
#' @export
logistic_map <- function(x){
  #Logistic function, maps (-inf, inf) to (0, 1).
  return(1.0 / (1.0 + exp(-x)))
}


#' Exponential distribution cdf, used to integrate intensity (0, inf) over 
#' time to a probability (0, 1)
#' @param intensity current_risk from spatial interaction model
#' @param dt timestep to integrate
#' @return probability of infection
#' @export
#' 
#' 
exp_cdf <- function(intensity, dt){
   return(1.0 - exp(-intensity * dt))
}

#' Calculating probabilities of becoming a COVID case
#'
#' Calculating probabilities of becoming a COVID case based on each individuals
#' 'current_risk'
#'
#' @param df The input list - the output from the create_input function
#' @param dt Time unit
#' @return An updated version of the input list with the probabilties updated
#' @export
infection_prob <- function(df, dt = 1){
  #Computes infection probability given exposure, time, demographics, and susceptibility params.
  #Args:
    #dt (float): The amount of time elapsed, always 1.0 in current model
  
    #exposure (float): The weighted sum of hazards at places this person visits
      # I THINK THIS WOULD BE THE CURRENT RISK COLUMN
  
    #beta_suscpetible (np.array): The regression weights mapping demographics to susceptibility.
    #demographics (np.array): All the demographic variables for this person.
      # THESE TWO TOGETHER IS ESSENTIALLY WHAT WE ARE DOING IN THE BETA SUMS FUNCTION, CORRECT?
  
  # First compute susceptibility, this is just a logistic regression
  #susceptibility <- logistic_map(x = np.dot(beta_susceptible, demographic))
  susceptibility <- 1
  
  exposure <- df$beta0 + df$betaxs
  # Then compute the infection prob for this time period
  infection_prob <- exp_cdf(intensity=(susceptibility * exposure), dt=dt)
  
  infection_prob[df$status %in% c(5,6)] <- 0 # if they are not susceptible then their probability is 0 of getting it
  infection_prob[df$status %in% c(1,2,3,4)] <- 1 # this makes keeping track of who has it easier
  
  df$probability <- infection_prob
  
  return(df)
}


#' Calculating probabilities of becoming a COVID case
#'
#' Calculating probabilities of becoming a COVID case based on each individuals
#' 'current_risk'
#'
#' @param df The input list - the output from the create_input function
#' @return An updated version of the input list with the probabilties updated
#' @export
covid_prob <- function(df){

  lpsi <- df$beta0 + df$betaxs

  psi <- exp(lpsi) / (exp(lpsi) + 1)
  psi <- normalizer(psi, 0,1,0.5,1)  # stretching out the probabilities to be between 0 and 1 rather than 0.5 and 1

  psi[df$status %in% c(5,6)] <- 0 # if they are not susceptible then their probability is 0 of getting it
  psi[df$status %in% c(1,2,3,4)] <- 1 # this makes keeping track of who has it easier
  df$probability <- psi

  return(df)
}

#' Assigns COVID cases based on individual probabilities
#'
#' Susceptible individuals are assigned COVID through a Bernoulli draw (rbinom)
#' based on their probability of becoming a COVID case.
#'
#' @param df Input list of the model - output of covid_prob function
#' @param tmp.dir Directory for saving a csv recording the number
#' of new cases each day
#' @param save_output Logical. Should the number of new cases be saved as output.
#' @param seed Seed for model run
#' @return An updated version of the input list with the new cases assigned
#' @importFrom withr with_seed
#' @export
case_assign <- function(df, 
                        tmp.dir = getwd(), 
                        save_output = TRUE,
                        seed = NULL){

  susceptible <- which(df$status == 0)

  df$new_status[susceptible] <- withr::with_seed(seed, stats::rbinom(n = length(susceptible),
                                       size = 1,
                                       prob = df$probability[susceptible]))


  if(file.exists("new_cases.csv")==FALSE) {
    ncase <- sum(df$new_status[susceptible])
  } else {
    ncase <- utils::read.csv("new_cases.csv")
    ncase$X <- NULL
    tmp <- sum(df$new_status[susceptible])
    ncase <- rbind(ncase,tmp)
    rownames(ncase) <- seq(1,nrow(ncase))
  }
  ncase <- as.data.frame(ncase)
  utils::write.csv(ncase, paste0(tmp.dir, "/new_cases.csv"))
  return(df)
}

#' Assign COVID cases by ranking individuals current risk
#'
#' Used to assign cases for the time period in which the model is being seeded.
#' Individuals are ranked in descending order by current risk and then desired number
#' of cases are assigned to the highest ranking individuals.
#'
#' Will be run in place of the case_assign function when the model is being seeded.
#'
#' @param df Input list of the model - output of covid_prob function
#' @param daily_case The desired number of cases that should be assigned on this day
#' @param seed Seed for model run
#' @importFrom withr with_seed
#' @return An updated version of the in put list with new rank assigned cases
#' added
#' @export
rank_assign <- function(df, 
                        daily_case,
                        seed = NULL){
  
  dfw <- data.frame(id = df$id, current_risk = df$current_risk, status = df$status)
  dfw <- dfw[dfw$status == 0,]
  max_risk <- dfw[dfw$current_risk == max(dfw$current_risk),]
  
  if(nrow(max_risk) > daily_case){
    rank_inf <- withr::with_seed(seed,sample(max_risk$id, size = daily_case, replace = FALSE))
  } else {
    rank_inf <- dfw[order(-dfw$current_risk),][1:daily_case,"id"]
  }
  inf_ind <- which(df$id %in% rank_inf)
  df$new_status[inf_ind] <- 1
  return(df)
}

#' Assigns the infection length of new cases
#'
#' Each new case is assigned a number of days an individual is both
#' presymptomatic and symptomatic for.
#'
#' @param df Input list of the function - output of an ____assign function
#' @param exposed_dist The distribution of the length of the exposed stage - options: weibull, lognormal, normal
#' @param exposed_mean The mean length of the exposed stage
#' @param exposed_sd The standard deviation of the length of the exposed stage
#' @param presymp_dist The distribution of the length of the presymptomatic stage - options: weibull, lognormal, normal
#' @param presymp_mean The mean length of the presymptomatic stage
#' @param presymp_sd The standard deviation of the length of the presymptomatic stage
#' @param infection_dist The distribution of the length of the symptomatic stage
#' @param infection_mean The mean length of the symptomatic stage
#' @param infection_sd The standard deviation of the length of the symptomatic stage
#' @param seed Seed for model run
#' @importFrom withr with_seed
#' @return An updated version of the input list with the new cases having
#' infection lengths assigned
#' @export
infection_length <- function(df, exposed_dist = "weibull",
                             exposed_mean = 3.2,
                             exposed_sd = 1,
                             presymp_dist = "weibull",
                             presymp_mean = 3.2,
                             presymp_sd = 1,
                             infection_dist = "normal",
                             infection_mean = 14,
                             infection_sd = 2,
                             seed = NULL){
  
  susceptible <- which(df$status == 0)
  
  new_cases <- which((df$new_status - df$status ==1) & df$status == 0)
  
  if (exposed_dist == "weibull"){
    wpar <- mixdist::weibullpar(mu = exposed_mean, sigma = exposed_sd, loc = 0)
    df$exposed_days[new_cases] <- round(withr::with_seed(seed,stats::rweibull(1:length(new_cases), shape = as.numeric(wpar["shape"]), scale = as.numeric(wpar["scale"]))),)
  }
  
  if (exposed_dist == "lognormal"){
    df$exposed_days[new_cases] <- round(withr::with_seed(seed,rlnorm(1:length(new_cases), meanlog = log(exposed_mean), sdlog = log(exposed_sd))))
  }
  
  if (exposed_dist == "normal"){
    df$exposed_days[new_cases] <- round(withr::with_seed(seed,stats::rnorm(1:length(new_cases), mean = exposed_mean, sd = exposed_sd)))
  }
  
  if (presymp_dist == "weibull"){
    wpar <- mixdist::weibullpar(mu = presymp_mean, sigma = presymp_sd, loc = 0)
    df$presymp_days[new_cases] <- round(withr::with_seed(seed,stats::rweibull(1:length(new_cases), shape = as.numeric(wpar["shape"]), scale = as.numeric(wpar["scale"]))),)
  }
  
  if (presymp_dist == "lognormal"){
    df$presymp_days[new_cases] <- round(withr::with_seed(seed,rlnorm(1:length(new_cases), meanlog = log(presymp_mean), sdlog = log(presymp_sd))))
  }
  
  if (presymp_dist == "normal"){
    df$presymp_days[new_cases] <- round(withr::with_seed(seed,stats::rnorm(1:length(new_cases), mean = presymp_mean, sd = presymp_sd)))
  }
  
  if (infection_dist == "weibull"){
    wpar <- mixdist::weibullpar(mu = infection_mean, sigma = infection_sd, loc = 0)
    df$symp_days[new_cases] <- round(withr::with_seed(seed,stats::rweibull(1:length(new_cases), shape = as.numeric(wpar["shape"]), scale = as.numeric(wpar["scale"]))),)
  }
  
  if (infection_dist == "normal"){
    df$symp_days[new_cases] <- round(withr::with_seed(seed,stats::rnorm(1:length(new_cases), mean = infection_mean, sd = infection_sd)))
  }
  
  if (infection_dist == "lognormal"){
    df$symp_days[new_cases] <- round(withr::with_seed(seed,stats::rnorm(1:length(new_cases), mean = log(infection_mean), sd = log(infection_sd))))
  }
  
  
  
  becoming_pre_sympt <- which((df$status == 1 | df$new_status == 1) & df$exposed_days == 0) ### maybe should be status rather than new_status
  
  # ovw_asymp <- which(df$BMIvg6[becoming_pre_sympt] %in% c("Overweight: 25 to less than 30",
  #                                                         "Obese I: 30 to less than 35",
  #                                                         "Obese II: 35 to less than 40",
  #                                                         "Obese III: 40 or more"))
  # 
  # symp_rates <-  rep(1 - asymp_rate, length(becoming_pre_sympt))
  # symp_rates[ovw_asymp] <-  symp_rates[ovw_asymp] * overweight_sympt_mplier
  # symp_rates[symp_rates > 1] <- 1
  # symp_rates[symp_rates < 0] <- 0
  
 
  symp_presymp <- withr::with_seed(seed,stats::rbinom(n = length(becoming_pre_sympt),
                                 size = 1,
                                 prob = df$sympt_risk[becoming_pre_sympt]))
  
  symp_presymp[symp_presymp == 0] <- 4
  symp_presymp[symp_presymp == 1] <- 2
  
  df$new_status[becoming_pre_sympt] <- symp_presymp 
  
  #ignoring the presymp days of asymp people - could add this onto the asymptomatic length?
  
  df$presymp_days[which(df$new_status == 4 | df$status == 4)] <- 0
  
  #switching people from being pre symptomatic to symptomatic and infected
  becoming_sympt <- which((df$status == 2 | df$new_status == 2) & df$presymp_days == 0) ### maybe should be status rather than new_status
  #df$new_status[becoming_sympt] <- 2
  
  df$new_status[becoming_sympt] <- 3
  
  return(df)
}


#' Removes cases
#'
#' For symptomatic individuals they have a 95% chance of recovery,
#' for asymptomatic they all recover.
#'
#' @param df Input list of the function - output of the infection_length function
#' @param chance_recovery Probability of an infected individual recovering
#' @param seed Seed for model run
#' @return An updated version of the input list with the status updates for those 
#' in their last day of being symptomatic/asymptomatic
#'
#' @export
removed <- function(df,
                    chance_recovery = 0.95,
                    seed = NULL){

  removed_cases_symp <- which(df$exposed_days == 0 & df$presymp_days == 0 & df$symp_days == 1 & 
                           (df$status == 3 | df$new_status == 3))
  
  removed_cases_asymp <- which(df$exposed_days == 0 & df$presymp_days == 0 & df$symp_days == 1 & 
                                 (df$status == 4 | df$new_status == 4))
  
  
  df$new_status[removed_cases_symp] <- 5 + withr::with_seed(seed,stats::rbinom(n = length(removed_cases_symp),
                                             size = 1,
                                             prob = (1-chance_recovery)))

  df$new_status[removed_cases_asymp] <- 5
  
   
  return(df)
}

#' Removes cases based on age
#' 
#' The survival rate of infected individuals decreases with age
#' 
#' @param df Input list of the function - output of the infection_length function.
#' Must contain an age item.
#' @param seed Seed for model run
#' @return An updated version of the input list with the status updates for those 
#' in their last day of being symptomatic/asymptomatic
#' @export
removed_age <- function(df,
                        seed = NULL){
  

      removed_cases_symp <- which(df$exposed_days == 0 & df$presymp_days == 0 & df$symp_days == 1 
                                  & (df$status == 3 | df$new_status == 3))
      
      removed_cases_asymp <- which(df$exposed_days == 0 & df$presymp_days == 0 & df$symp_days == 1 & 
                                     (df$status == 4 | df$new_status == 4))
      
      df$new_status[removed_cases_symp] <- 5 + withr::with_seed(seed,stats::rbinom(n = length(removed_cases_symp),
                                                             size = 1,
                                                             prob = df$mortality_risk))

      df$new_status[removed_cases_asymp] <- 5
    return(df)
}


#' Recalculates number of symptomatic and presymptomatic days remaining
#'
#' @param df Input list of the function - output of the removed function
#' @return An updated version of the input list with the status updates for those
#' days left in stage = 0.
#' @export
recalc_sympdays <- function(df){
  
  #df$symp_days[removed_cases] <- 0
  df$exposed_days[df$exposed_days > 0 & df$status == 1] <- df$exposed_days[df$exposed_days > 0 & df$status == 1] - 1
  df$presymp_days[df$presymp_days > 0 & df$status  == 2 ] <- df$presymp_days[df$presymp_days > 0  & df$status  == 2] - 1
  df$symp_days[df$symp_days > 0  & (df$status == 3 |df$status == 4 )] <- df$symp_days[df$symp_days > 0 & (df$status == 3 |df$status == 4 )] - 1
  
  return(df)
}

#' Runs the pipeline for removing cases and stepping symptomatic and presymptomatic days
#'
#' @param df Input list of the function - output of the infection_length function
#' @param chance_recovery Probability of an infected individual recovering
#' @param seed Seed for model run
#' @return An updated version of the input list with the status updates for those
#' days left in stage = 0.
#' @export
run_removal_recalc <- function(df, 
                               chance_recovery = 0.95, 
                               seed = NULL){
  df_tmp <- removed(df, chance_recovery = chance_recovery, seed = seed)
  df_tmp <- recalc_sympdays(df_tmp)
  return(df_tmp)
}


#' Changing the spread of values to be between two set values
#'
#' For use in the covid_prob function to change the probabilities
#' to be 0-1 rather than 0.5-1
#'
#' @param x A number or vector of numbers
#' @param lower_bound Desired lower_bound of values
#' @param upper_bound Desired upper_bound of values
#' @param xmin Expected minimum value of x
#' @param xmax Expected maximum value of x
#' @return A number or vector of numbers between the lower and upper bounds
#' @export
normalizer <- function(x, 
                       lower_bound, 
                       upper_bound, 
                       xmin, 
                       xmax){

  normx <-  (upper_bound - lower_bound) * (x - xmin)/(xmax - xmin) + lower_bound
  return(normx)
}
