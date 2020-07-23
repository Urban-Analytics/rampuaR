#' Downloading Google Mobility Data
#' 
#' @param force_gm Logical. Should the data be downloaded even if it 
#' already exists
#' @export
gm_file_download <- function(force_gm = FALSE){
  
  if((force_gm == FALSE &
      !file.exists("Google_Global_Mobility_Report.csv")) | force_gm == TRUE){
    utils::download.file(
      "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
      destfile = "Google_Global_Mobility_Report.csv"
    )
  }
}

#' Downloading crosswalk table of MSOA and LAD codes
#' 
#' @param force_lad Logical. Should the data be downloaded even if it already exists
#' 
#' @export
lad_file_download <- function(force_lad = FALSE){
  
  if((force_lad == FALSE &
      !file.exists("lad_codes.csv")) | force_lad == TRUE){
    utils::download.file(
      "http://geoportal1-ons.opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv",
      "lad_codes.csv")
  }
}

#' Downloading a crosswalk table of county and LAD codes
#' 
#' @param force_county Logical. Should the data be downloaded even if there is
#' an existing download
#' @export
county_file_download <- function(force_county = FALSE){
  
  if((force_county == FALSE &
      !file.exists("lad_county_codes.csv")) | force_county == TRUE){
    utils::download.file(
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
#' @export
msoa_lad_code_matcher <- function(pop, lad_codes){
  
  code_match <- lad_codes %>% 
    dplyr::filter(MSOA11CD %in% pop$area) %>% 
    dplyr::select(lad_name = LAD17NM, lad_code = LAD17CD, county = CTYUA16NM) %>% 
    dplyr::distinct() 
  
  pop_out <- data.frame(pop, code_match)
  
  return(pop_out)
}

#' Matching LAD/County to Google Mobility Region
#' 
#' Matching the name of the LAD/County to the closest matching Google Mobility 
#' Region
#' 
#' @param lad_string The LAD to try and match to Google Mobility region
#' @param county_string The county to try and match to Google Mobility region
#' @param strings_to_match Vector of sub-regions from the Google Mobility Data
#' @return The name of the Google Mobility sub-region that most closely
#'  matches the LAD/county name
#'  @export
closest_string <- function(lad_string, county_string, strings_to_match){
  
  lad_string <- gsub(", City of", "", lad_string) # Hull causing grief
  
  lad_max_mat <- max(RecordLinkage::levenshteinSim(lad_string,strings_to_match))
  county_max_mat <- max(RecordLinkage::levenshteinSim(county_string, strings_to_match))
  
  if (lad_max_mat >= county_max_mat){
    lad_max_which <- which.max(RecordLinkage::levenshteinSim(lad_string,strings_to_match))
    return(strings_to_match[lad_max_which])
  }
  
  if (county_max_mat > lad_max_mat){
    county_max_which <- which.max(RecordLinkage::levenshteinSim(county_string,strings_to_match))
    return(strings_to_match[county_max_which])
  } else{
    return("")
  }
}


#' Filtering out the Google Mobility for the LAD or county
#' 
#' @param gm Google Mobility data - output of gm_file_download.
#' @param lad_name Name of LAD which we want Google Mobility data for.
#' @param county_name Name of county which we want Google Mobility data for, 
#' lad_name should be geographically within county_name.
#' 
#' @return Google Mobility data for the given LAD or county
#' @export
gm_filter <- function(gm, lad_name, county_name){
  
  gm_filt <- gm[gm$sub_region_1 == closest_string(lad_string = lad_name,
                                                  county_string = county_name,
                                                  strings_to_match = gm$sub_region_1),]
  
  if(nrow(gm_filt) == 0){
    gm_filt <- paste0("No matching Google Mobility data for ", name)
  }
  
  return(gm_filt)  
}

#' Format Google Mobility data
#' 
#' Format the Google Mobility output to long format 
#' and looking at only the "residential_percent_change_from_baseline" data
#' 
#' @param gm_filt Output from the gm_filter function
#' @return Long format data with the day and value for the
#'  residential change from baseline
#' @export
format_gm <- function(gm_filt){
  
  residential_pcnt <- gm_filt %>% 
    tidyr::pivot_longer(., contains("percent")) %>% 
    dplyr::filter(
      name == "residential_percent_change_from_baseline"
    ) %>% 
    dplyr::mutate(day = as.numeric(date) - 18306) %>% #February 15th as day 1
    dplyr::select(day, value) 
  
  return(residential_pcnt)
}


#' Smoothing the Google Mobility Residential data - there are some days with missing data
#' 
#' @param residential_pcnt Google Mobility residential change 
#' from baseline data
#' @return A smoothed set of values for the amount of time people spend 
#' in residential locations relative to the baseline
#' @export
residential_smoother <- function(residential_pcnt){

  smooth_residential <- mgcv::gam(value ~ s(day, bs = "cr"), fx = TRUE, data = residential_pcnt)
  new_data <- data.frame(day = 1:nrow(residential_pcnt), value = 0)
  sr <- stats::predict(smooth_residential, new_data, type = "response")
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
