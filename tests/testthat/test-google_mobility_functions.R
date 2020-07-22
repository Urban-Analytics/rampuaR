test_that("gm_file_download works", {
  
  utils::download.file(
    "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", "temp_file.csv")
  
  expect_true(file.exists("temp_file.csv"))
  expect_true(file.size("temp_file.csv") > 0)
  
  file.remove("temp_file.csv")
})


test_that("lad_file_download works", {
  
  utils::download.file(
    "http://geoportal1-ons.opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv", "temp_file.csv")
  
  expect_true(file.exists("temp_file.csv"))
  expect_true(file.size("temp_file.csv") > 0)
  
  file.remove("temp_file.csv")
})

test_that("county_file_download works", {
  
  utils::download.file(
    "https://opendata.arcgis.com/datasets/46dea3d10fc44da9b8daf19ca6f2c204_0.csv", "temp_file.csv")
  
  expect_true(file.exists("temp_file.csv"))
  expect_true(file.size("temp_file.csv") > 0)
  
  file.remove("temp_file.csv")
})


test_that("msoa_lad_code_matcher works", {
  
  pop <- data.frame(id = 1:10, area = "f")
  
  lad_codes <- data.frame(MSOA11CD = letters[seq( from = 1, to = 10 )], 
                          LAD17NM = c("blue","yellow", "green", "red", "pink", "orange", "purple", "red", "brown", "black"),
                          LAD17CD = sample(1:1000, 10),
                          CTYUA16NM = c("banana", "apple", "mango", "strawberry", "pear", "cherry", "orange", "melon", "lemon", "lime"))
  
  expect_gte(ncol(msoa_lad_code_matcher(pop = pop, lad_codes = lad_codes)), ncol(pop))
  expect_type(msoa_lad_code_matcher(pop = pop, lad_codes = lad_codes)[["county"]], "character")
  
})


test_that("closest_string works",{
  
  lad_string <- "Blaby"
  county_string <- "Leicestershire"
  strings_to_match <- c("Leicestershire", "Leicester", "Gloucestershire")
  
  expect_equal(closest_string(lad_string = lad_string, 
                 county_string = county_string,
                 strings_to_match = strings_to_match), "Leicestershire")
  
  lad_string <- "Leicester"
  
  expect_equal(closest_string(lad_string = lad_string, 
                              county_string = county_string,
                              strings_to_match = strings_to_match), "Leicester")
  
})

test_that("gm_filter works", {
  
  utils::download.file(
    "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", "temp_file.csv")
  gm <- read.csv("temp_file.csv")
  
  lad_name <- "Blaby"
  county_name <- "Leicestershire"
  
  expect_lt(nrow(gm_filter(gm = gm,
            lad_name = lad_name, 
            county_name = county_name)), nrow(gm))
  
  expect_gt(nrow(gm_filter(gm = gm,
                           lad_name = lad_name, 
                           county_name = county_name)), 0)
  
  file.remove("temp_file.csv")
})


test_that("format_gm works", {
  
  df <- data.frame(date = 18307:18407, residential_percent_change_from_baseline = cospi(seq(0,1.7, length.out = 101))*100+ runif(101, -30,30))
  
  expect_equal(ncol(format_gm(gm_filt = df)), 2)

  })



test_that("residential_smoother works", {
  
  df <- data.frame(day = 1:100, value = cospi(seq(0,1.7, length.out = 100))*100+ runif(100, -30,30))
  
  expect_equal(length(residential_smoother(residential_pcnt = df)), nrow(df))
  expect_type(residential_smoother(residential_pcnt = df), "double")

  })

test_that("lockdown_multiplier works", {

  a <- runif(10, 0,1)
  b <- 1-a
  df <- data.frame(phome = a, pnothome = b, lad_name = "Blaby", lad_code = 123)
  sr <- (sinpi(seq(-0.5,1, length.out = 100))*0.1)+1 
  
  expect_equal(ncol(lockdown_multiplier(smth_res = sr, pop = df)), 4)
  expect_equal(nrow(lockdown_multiplier(smth_res = sr, pop = df)), length(sr))
  
  })



