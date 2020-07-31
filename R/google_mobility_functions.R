#' Downloading Google Mobility Data
#' 
#' @param force_gm Logical. Should the data be downloaded even if it 
#' already exists
#' @importFrom utils download.file
#' @export
gm_file_download <- function(force_gm = FALSE){
  
  if((force_gm == FALSE &
      !file.exists("Google_Global_Mobility_Report.csv")) | force_gm == TRUE){
    download.file(
      "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
      destfile = "Google_Global_Mobility_Report.csv"
    )
  }
}

#' Downloading crosswalk table of MSOA and LAD codes
#' 
#' @param force_lad Logical. Should the data be downloaded even if it already exists
#' 
#' @importFrom utils download.file
#' @export
lad_file_download <- function(force_lad = FALSE){
  
  if((force_lad == FALSE &
      !file.exists("lad_codes.csv")) | force_lad == TRUE){
    download.file(
      "http://geoportal1-ons.opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv",
      "lad_codes.csv")
  }
}

#' Downloading a crosswalk table of county and LAD codes
#' 
#' @param force_county Logical. Should the data be downloaded even if there is
#' an existing download
#' @importFrom utils download.file
#' @export
county_file_download <- function(force_county = FALSE){
  
  if((force_county == FALSE &
      !file.exists("lad_county_codes.csv")) | force_county == TRUE){
   download.file(
      "https://opendata.arcgis.com/datasets/46dea3d10fc44da9b8daf19ca6f2c204_0.csv",
      "lad_county_codes.csv")
  }
}

#' Matching the LAD level populations with their associated LAD and county names
#' 
#' @param pop An LAD level population with MSOA area codes
#' @param lad_codes Crosswalk of MSOA, LAD codes and county names
#' 
#' @return The population with added LAD and county names
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @export
msoa_lad_code_matcher <- function(pop, lad_codes){

  if(typeof(lad_codes$CTYUA16NM) != "character"){
    stop("Error: the CTYUA16NM column data type is not character.")
  }
  
  code_match <- lad_codes %>% 
   filter(MSOA11CD %in% pop$area) %>% 
    select(lad_name = LAD17NM, lad_code = LAD17CD, county = CTYUA16NM) %>% 
    distinct() 
  
  pop_out <- data.frame(pop, code_match)
  
  return(pop_out)
}


#' Format Google Mobility data
#' 
#' Format the Google Mobility output to long format 
#' and looking at only the "residential_percent_change_from_baseline" data
#' 
#' @param gm_filt Output from the gm_filter function
#' @return Long format data with the day and value for the
#'  residential change from baseline
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr %>% 
#' @importFrom tidyselect contains
#' 
#' @export
format_gm <- function(gm_filt){
  
  residential_pcnt <- gm_filt %>% 
    pivot_longer(., contains("percent")) %>% 
   filter(
      name == "residential_percent_change_from_baseline"
    ) %>% 
    mutate(day = as.numeric(date) - 18306) %>% #February 15th as day 1
   select(day, value) 
  
  return(residential_pcnt)
}


#' Smoothing the Google Mobility Residential data - there are some days with missing data
#' 
#' @param residential_pcnt Google Mobility residential change 
#' from baseline data
#' @return A smoothed set of values for the amount of time people spend 
#' in residential locations relative to the baseline
#' @importFrom mgcv gam
#' @importFrom stats predict
#' @export
residential_smoother <- function(residential_pcnt){

  smooth_residential <- gam(value ~ s(day, bs = "cr"), fx = TRUE, data = residential_pcnt)
  new_data <- data.frame(day = 1:nrow(residential_pcnt), value = 0)
  sr <- predict(smooth_residential, new_data, type = "response")
  sr <- (sr/100) + 1
  
  return(sr)
}

#' Converting the smoothed residential data into a multiplier for time outside
#' 
#' @param smth_res A numeric vector showing the (smoothed) proportion of time 
#' people have spent in residential locations
#' @param pop The output from the msoa_lad_code_matcher function
#' @export
lockdown_multiplier <- function(smth_res, pop){
  
  new_out <- 1 - (mean(pop$phome, na.rm = TRUE) * smth_res) # During the first two weeks of lockdown people spent 21% more time at home - the average person from devon spent ~ 75% of their time at home before lockdow, during lockdown this increases to 92% - meaning about 8% of time outside the home on average
  
  lock_down_reducer <-  new_out/mean(pop$pnothome) # percentage of time spent outside of compared to pre-lockdown
  plot(lock_down_reducer, main = unique(pop$lad_name))
  
  daily_lock_down_multiplier <-  data.frame(lad_name = unique(pop$lad_name),
                                            lad_code = unique(pop$lad_code),
                                            day = 1:length(lock_down_reducer),
                                            timeout_multiplier = lock_down_reducer)
  
  return(daily_lock_down_multiplier)
}


