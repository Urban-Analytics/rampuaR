test_that("create_input works",{

  df <- data.frame(presymp_days = sample(1:10, size = 10, replace = TRUE),
                   symp_days = sample(5:15, size = 10, replace = TRUE),
                   disease_status = c(rep(0,5), 1, 2, 3, 4, 0),
                   current_risk  = runif(10, 0, 1))

  expect_type(create_input(micro_sim_pop = df,vars = "current_risk"), "list")
  expect_type(create_input(micro_sim_pop = df,vars = "current_risk")[["current_risk"]], "double")
  expect_type(create_input(micro_sim_pop = df,vars = "current_risk")[["beta0"]], "double")
  expect_type(create_input(micro_sim_pop = df,vars = "current_risk")[["betaxs"]], "double")
  expect_type(create_input(micro_sim_pop = df,vars = "current_risk")[["hid_status"]], "double")
  expect_type(create_input(micro_sim_pop = df,vars = "current_risk")[["presymp_days"]], "integer")
  expect_type(create_input(micro_sim_pop = df,vars = "current_risk")[["symp_days"]], "integer")
  expect_type(create_input(micro_sim_pop = df,vars = "current_risk")[["probability"]], "double")
  expect_type(create_input(micro_sim_pop = df,vars = "current_risk")[["status"]], "integer")
  expect_type(create_input(micro_sim_pop = df,vars = "current_risk")[["new_status"]], "integer")

  expect_length(create_input(micro_sim_pop = df,vars = "current_risk")[["current_risk"]], nrow(df))
  expect_length(create_input(micro_sim_pop = df,vars = "current_risk")[["beta0"]], nrow(df))
  expect_length(create_input(micro_sim_pop = df,vars = "current_risk")[["betaxs"]], nrow(df))
  expect_length(create_input(micro_sim_pop = df,vars = "current_risk")[["hid_status"]], nrow(df))
  expect_length(create_input(micro_sim_pop = df,vars = "current_risk")[["presymp_days"]], nrow(df))
  expect_length(create_input(micro_sim_pop = df,vars = "current_risk")[["symp_days"]], nrow(df))
  expect_length(create_input(micro_sim_pop = df,vars = "current_risk")[["probability"]], nrow(df))
  expect_length(create_input(micro_sim_pop = df,vars = "current_risk")[["status"]], nrow(df))
  expect_length(create_input(micro_sim_pop = df,vars = "current_risk")[["new_status"]], nrow(df))

})


test_that("mortality risk works", {
  
  df <- list(id = sample(1:100, 10, replace = FALSE),
             age = sample(1:100, 10, replace = FALSE),
             BMIvg6 = sample(0:5, 10,replace = TRUE),
             cvd = sample(0:1, 10,replace = TRUE),
             diabetes = sample(0:1, 10,replace = TRUE),
             bloodpressure = sample(0:1, 10,replace = TRUE),
             mortality_rate = rep(0,10))
  
 # expect_gt(sum(mortality_risk(df, obesity_40 = 1.9, cvd = 2, diabetes = 2, bloodpressure = 1.5)[["mortality_risk"]]), sum(df$mortality_risk))
  expect_equal(mortality_risk(df, obesity_40 = 1.9, cvd = 2, diabetes = 2, bloodpressure = 1.5)[["id"]], df$id)
  expect_equal(mortality_risk(df, obesity_40 = 1.9, cvd = 2, diabetes = 2, bloodpressure = 1.5)[["age"]], df$age)
  expect_equal(mortality_risk(df, obesity_40 = 1.9, cvd = 2, diabetes = 2, bloodpressure = 1.5)[["obese40"]], df$obese40)
  expect_equal(mortality_risk(df, obesity_40 = 1.9, cvd = 2, diabetes = 2, bloodpressure = 1.5)[["cvd"]], df$cvd)
  expect_equal(mortality_risk(df, obesity_40 = 1.9, cvd = 2, diabetes = 2, bloodpressure = 1.5)[["diabetes"]], df$diabetes)
  expect_equal(mortality_risk(df, obesity_40 = 1.9, cvd = 2, diabetes = 2, bloodpressure = 1.5)[["bloodpressure"]], df$bloodpressure)
  
})

test_that("sum_betas", {
  
  df <- list(id = sample(1:100, 10, replace = FALSE),
             current_risk = runif(10, 0, 1),
             beta0 = rep(0, 10),
             betaxs = rep(0, 10),
             hid_status = rep(0, 10),
             presymp_days = sample(1:10, size = 10, replace = TRUE),
             symp_days = sample(5:15, size = 10, replace = TRUE),
             status = c(rep(0,5), 1, 2, 3, 4, 0),
             new_status = c(rep(0,5), 1, 2, 3, 4, 0))
  
  expect_type(sum_betas(df = df, betas = list(current_risk = 0.42), risk_cap_val = 5), "list")
  expect_length(sum_betas(df = df, betas = list(current_risk = 0.42), risk_cap_val = 5)[["betaxs"]], length(df$betaxs))
  
})


test_that("covid_prob", {

  df <- list(id = sample(1:100, 10, replace = FALSE),
              current_risk = runif(10, 0, 1),
              beta0 = rep(0, 10),
              betaxs = rep(0, 10),
              hid_status = rep(0, 10),
              presymp_days = sample(1:10, size = 10, replace = TRUE),
              symp_days = sample(5:15, size = 10, replace = TRUE),
              status = c(rep(0,5), 1, 2, 3, 4, 0),
              new_status = c(rep(0,5), 1, 2, 3, 4, 0))

  expect_equal(covid_prob(df = df)[["id"]], df$id)
  expect_equal(covid_prob(df = df)[["current_risk"]], df$current_risk)
  expect_equal(covid_prob(df = df)[["beta0"]], df$beta0)
  expect_equal(covid_prob(df = df)[["hid_status"]], df$hid_status)
  expect_equal(covid_prob(df = df)[["presymp_days"]], df$presymp_days)
  expect_equal(covid_prob(df = df)[["symp_days"]], df$symp_days)
  expect_equal(covid_prob(df = df)[["status"]], df$status)
  expect_equal(covid_prob(df = df)[["new_status"]], df$new_status)
  
  expect_type(covid_prob(df = df)[["betaxs"]], "double")
  
  expect_length(covid_prob(df = df)[["betaxs"]], length(df$betaxs))
  
  expect_type(covid_prob(df = df)[["probability"]], "double")
  expect_length(covid_prob(df = df)[["probability"]], length(df$betaxs))

  expect_lte(max(covid_prob(df = df)[["probability"]]), 1)
  expect_gte(min(covid_prob(df = df)[["probability"]]), 0)
})

test_that("case_assign works", {

  df <- list(id = sample(1:100, 10, replace = FALSE),
             current_risk = runif(10, 0, 1),
             beta0 = rep(0, 10),
             betaxs = runif(10, 0, 1),
             hid_status = rep(0, 10),
             presymp_days = sample(1:10, size = 10, replace = TRUE),
             symp_days = sample(5:15, size = 10, replace = TRUE),
             status = c(rep(0,5), 1, 2, 3, 4, 0),
             new_status = c(rep(0,5), 1, 2, 3, 4, 0),
             probability = runif(10, 0, 1)
             )

  seed = sample(1:100, 1)
  
  expect_true(all(case_assign(df, seed = seed)[["new_status"]] >= df$new_status))
  expect_true(all(case_assign(df, seed = seed)[["new_status"]] >= df$status))

  expect_type(case_assign(df, seed = seed), "list")

  expect_equal(case_assign(df, seed = seed)[["id"]], df$id)
  expect_equal(case_assign(df, seed = seed)[["current_risk"]], df$current_risk)
  expect_equal(case_assign(df, seed = seed)[["beta0"]], df$beta0)
  expect_equal(case_assign(df, seed = seed)[["betaxs"]], df$betaxs)
  expect_equal(case_assign(df, seed = seed)[["hid_status"]], df$hid_status)
  expect_equal(case_assign(df, seed = seed)[["presymp_days"]], df$presymp_days)
  expect_equal(case_assign(df, seed = seed)[["symp_days"]], df$symp_days)
  expect_equal(case_assign(df, seed = seed)[["status"]], df$status)
  expect_equal(case_assign(df, seed = seed)[["probability"]], df$probability)
})


test_that("rank_assign works", {
  df <- list(id = sample(1:100, 10, replace = FALSE),
             current_risk = runif(10, 0, 1),
             beta0 = rep(0, 10),
             betaxs = runif(10, 0, 1),
             hid_status = rep(0, 10),
             presymp_days = sample(1:10, size = 10, replace = TRUE),
             symp_days = sample(5:15, size = 10, replace = TRUE),
             status = c(rep(0,5), 1, 2, 3, 4, 0),
             new_status = c(rep(0,5), 1, 2, 3, 4, 0),
             probability = runif(10, 0, 1))

  daily_case <- 10

  expect_true(all(rank_assign(df, daily_case = daily_case)[["new_status"]] >= df$new_status))
  expect_true(all(rank_assign(df, daily_case = daily_case)[["new_status"]] >= df$status))

  expect_type(rank_assign(df, daily_case = daily_case), "list")

  expect_equal(rank_assign(df, daily_case = daily_case)[["id"]], df$id)
  expect_equal(rank_assign(df, daily_case = daily_case)[["current_risk"]], df$current_risk)
  expect_equal(rank_assign(df, daily_case = daily_case)[["beta0"]], df$beta0)
  expect_equal(rank_assign(df, daily_case = daily_case)[["betaxs"]], df$betaxs)
  expect_equal(rank_assign(df, daily_case = daily_case)[["hid_status"]], df$hid_status)
  expect_equal(rank_assign(df, daily_case = daily_case)[["presymp_days"]], df$presymp_days)
  expect_equal(rank_assign(df, daily_case = daily_case)[["symp_days"]], df$symp_days)
  expect_equal(rank_assign(df, daily_case = daily_case)[["status"]], df$status)
  expect_equal(rank_assign(df, daily_case = daily_case)[["probability"]], df$probability)
})


test_that("infection_length works", {

  df <- list(id = 1:10,
             current_risk = runif(10, 0, 1),
             beta0 = rep(0, 10),
             betaxs = runif(10, 0, 1),
             hid_status = rep(0, 10),
             presymp_days = rep(0,10),
             symp_days = rep(0,10),
             status = c(rep(0,5), 1, 2, 3, 4, 0),
             new_status = c(rep(1,5), 2, 3, 4, 4, 0),
             probability = runif(10, 0, 1),
             sympt_risk = runif(10, 0, 1))

  seed = sample(1:100, 1)
  
  timestep <- 1

  expect_type(infection_length(df, seed = seed), "list")

  expect_true(all(infection_length(df, seed = seed)[["new_status"]] >= df$new_status))
  expect_true(all(infection_length(df, seed = seed)[["new_status"]] >= df$status))

  expect_equal(infection_length(df, seed = seed)[["id"]], df$id)
  expect_equal(infection_length(df, seed = seed)[["current_risk"]], df$current_risk)
  expect_equal(infection_length(df, seed = seed)[["beta0"]], df$beta0)
  expect_equal(infection_length(df, seed = seed)[["betaxs"]], df$betaxs)
  expect_equal(infection_length(df, seed = seed)[["hid_status"]], df$hid_status)
  expect_equal(infection_length(df, seed = seed)[["status"]], df$status)
  expect_equal(infection_length(df, seed = seed)[["probability"]], df$probability)
  expect_equal(sum(infection_length(df, seed = seed)[["symp_days"]] >0) - sum(df$symp_days > 0),sum(df$status == 0 & df$new_status == 1))

})


test_that("removed works", {

  df <- list(id = 1:10,
             current_risk = runif(10, 0, 1),
             beta0 = rep(0, 10),
             betaxs = runif(10, 0, 1),
             hid_status = rep(0, 10),
             presymp_days = rep(0,10),
             symp_days = rep(1,10),
             status = c(rep(0,5), rep(2, 5)),
             new_status =c(rep(0,5), rep(2, 5)),
             probability = runif(10, 0, 1))
  
  seed = sample(1:100, 1)
  timestep <- 1
  expect_type(removed(df, seed = seed), "list")

  expect_true(all(removed(df, seed = seed)[["new_status"]] >= df$status))

})


test_that("removed_age works", {
  df <- list(id = 1:100,
             Age1 = sample(1:100, 100),
             exposed_days = rep(0,100),
             presymp_days = rep(0,100),
             symp_days = rep(1,100),
             status =c(rep(3,50), rep(4, 50)),
             new_status =c(rep(3,50), rep(4, 50)),
             mortality_risk = runif(100, 0, 0.2))
  
  seed = sample(1:100, 1)
  
  expect_true(all(removed_age(df, seed = seed)[["new_status"]] >= df$status))
})


test_that("recalc_sympdays works", {

  df <- list(id = 1:10,
             current_risk = runif(10, 0, 1),
             beta0 = rep(0, 10),
             betaxs = runif(10, 0, 1),
             hid_status = rep(0, 10),
             presymp_days = rep(0,10),
             symp_days = rep(1,10),
             status = c(rep(0,5), rep(2, 5)),
             new_status =c(rep(0,5), rep(2, 5)),
             probability = runif(10, 0, 1))
  timestep <- 1
  expect_type(recalc_sympdays(df), "list")
  
  expect_true(all(recalc_sympdays(df)[["new_status"]] >= df$new_status))

})

test_that("run_removal_recalc works", {
  
  df <- list(id = 1:10,
             current_risk = runif(10, 0, 1),
             beta0 = rep(0, 10),
             betaxs = runif(10, 0, 1),
             hid_status = rep(0, 10),
             presymp_days = rep(0,10),
             symp_days = rep(1,10),
             status = c(rep(0,5), rep(2, 5)),
             new_status =c(rep(0,5), rep(2, 5)),
             probability = runif(10, 0, 1))
  
  seed <- sample(1:100, 1)
  
  timestep <- 1

  expect_true(all(run_removal_recalc(df, chance_recovery = 0.95, seed = seed)[["new_status"]] >= df$new_status))
  
})

test_that("normalizer works", {
  expect_equal(normalizer(0.5, 0,1,0.5,1), 0)
  expect_equal(normalizer(0.75, 0,1,0.5,1), 0.5)
  expect_equal(normalizer(1, 0,1,0.5,1), 1)
  expect_equal(normalizer(0, 0, 1, 0.5, 1), -1)
})

test_that("logistic_map works", {
  expect_lte(max(logistic_map(runif(1000, min = -99999999999, max = 99999999999))), 1)
  expect_gte(min(logistic_map(runif(1000, min = -99999999999, max = 99999999999))), 0)
})


test_that("exp_cdf works", {
  expect_lte(max(exp_cdf(rexp(10000, 5), dt = 1)), 1)
  expect_gte(min(exp_cdf(rexp(10000, 5), dt = 1)), 0)
})


test_that("infection prob works", {
  
  df <- data.frame(beta0 = rep(0, 100),
                   betaxs = rexp(100, 1),
                   status = rep(0, 100),
                   probability = NA)
  
  expect_lte(sum(infection_prob(df, dt = 1)[["probability"]]), nrow(df))
  expect_equal(sum(infection_prob(df, dt = 1)[["status"]]), sum(df$status))
  expect_equal(sum(infection_prob(df, dt = 1)[["betaxs"]]), sum(df$betaxs))
  expect_equal(sum(infection_prob(df, dt = 1)[["beta0"]]), sum(df$beta0))
  
})



test_that("vaccinate works",{
  
  df <- data.frame(age = sample(1:100, 100, replace = TRUE),
                   new_status = rep(0,100),
                   exposed_days = sample(0:3, 100, replace = TRUE),
                   presymp_days = sample(0:3, 100, replace = TRUE),
                   symp_days = sample(0:7,100, replace = TRUE))

  expect_gte(sum(vaccinate(df)[["new_status"]]),sum(df$new_status))  
  
  
})

test_that("age_symp_risk works", {
  
  age <- c(10, 40, 70)
  
  expect_equal(age_symp_risk(age[1]), 0.21)
  expect_equal(age_symp_risk(age[2]), 0.45)
  expect_equal(age_symp_risk(age[3]), 0.69)

    })

test_that("sympt_risk works", {
  
  df <- data.frame(age = sample(1:100, 100, replace = TRUE),
                   sympt_risk = NA,
                   BMIvg6 = sample(c("Underweight: less than 18.5",
                                     "Normal: 18.5 to less than 25",
                                     "Not applicable",
                                     "Obese I: 30 to less than 35",
                                     "Obese II: 35 to less than 40",
                                     "Obese III: 40 or more",
                                     "Overweight: 25 to less than 30"), 100, replace = TRUE),
                   diabetes = rbinom(100, 1, 0.1),
                   bloodpressure = rbinom(100, 1, 0.1),
                   cvd = rbinom(100, 1, 0.1))

  expect_gte(sum(sympt_risk(df)[["sympt_risk"]]), 0)  
  
})


