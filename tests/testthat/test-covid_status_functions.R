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
             probability = runif(10, 0, 1))

  expect_true(all(case_assign(df)[["new_status"]] >= df$new_status))
  expect_true(all(case_assign(df)[["new_status"]] >= df$status))

  expect_type(case_assign(df), "list")

  expect_equal(case_assign(df)[["id"]], df$id)
  expect_equal(case_assign(df)[["current_risk"]], df$current_risk)
  expect_equal(case_assign(df)[["beta0"]], df$beta0)
  expect_equal(case_assign(df)[["betaxs"]], df$betaxs)
  expect_equal(case_assign(df)[["hid_status"]], df$hid_status)
  expect_equal(case_assign(df)[["presymp_days"]], df$presymp_days)
  expect_equal(case_assign(df)[["symp_days"]], df$symp_days)
  expect_equal(case_assign(df)[["status"]], df$status)
  expect_equal(case_assign(df)[["probability"]], df$probability)
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
             probability = runif(10, 0, 1))

  timestep <- 1

  expect_type(infection_length(df), "list")

  expect_true(all(infection_length(df)[["new_status"]] >= df$new_status))
  expect_true(all(infection_length(df)[["new_status"]] >= df$status))

  expect_equal(infection_length(df)[["id"]], df$id)
  expect_equal(infection_length(df)[["current_risk"]], df$current_risk)
  expect_equal(infection_length(df)[["beta0"]], df$beta0)
  expect_equal(infection_length(df)[["betaxs"]], df$betaxs)
  expect_equal(infection_length(df)[["hid_status"]], df$hid_status)
  expect_equal(infection_length(df)[["status"]], df$status)
  expect_equal(infection_length(df)[["probability"]], df$probability)
  expect_equal(sum(infection_length(df)[["presymp_days"]] >0) - sum(df$presymp_days > 0),sum(df$status == 0 & df$new_status == 1))

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
  timestep <- 1
  expect_type(removed(df), "list")

  expect_true(all(removed(df)[["new_status"]] >= df$status))

})


test_that("removed_age works", {
  df <- list(id = 1:100,
             age = sample(1:100, 100),
             exposed_days = rep(0,100),
             presymp_days = rep(0,100),
             symp_days = rep(1,100),
             status =c(rep(3,50), rep(4, 50)),
             new_status =c(rep(3,50), rep(4, 50)))
  
  removed_age(df)
  
  expect_true(all(removed_age(df)[["new_status"]] >= df$status))
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
  timestep <- 1

  expect_true(all(run_removal_recalc(df, chance_recovery = 0.95)[["new_status"]] >= df$new_status))
  
})

test_that("normalizer works", {
  expect_equal(normalizer(0.5, 0,1,0.5,1), 0)
  expect_equal(normalizer(0.75, 0,1,0.5,1), 0.5)
  expect_equal(normalizer(1, 0,1,0.5,1), 1)
  expect_equal(normalizer(0, 0, 1, 0.5, 1), -1)
})


test_that("run_status works", {
  
  df <- data.frame(area = sample(c("E02004143","E02004144","E02004149","E02004154","E02004157","E02004159","E02004162","E02004163","E02004208"), 100000, replace = TRUE),
                   Age1 = sample(1:10, 100000, replace = TRUE),
                   Sex = sample(0:1, 100000, replace = TRUE),
                   current_risk = runif(100, 0, 100000),
                   pnothome = rnorm(100000, 0.7, 0.2),
                   disease_status = sample(0:6, 100000, replace = TRUE),
                   exposed_days = sample(0:4, 100000, replace = TRUE),
                   presymp_days = sample(0:4, 100000, replace = TRUE),
                   symp_days = sample(0:21, 100000, replace = TRUE),
                   id = 1:100000,
                   house_id = sample(1:25, 100000, replace = TRUE))
  
  expect_true(nrow(run_status(pop = df)) == nrow(df))
})


