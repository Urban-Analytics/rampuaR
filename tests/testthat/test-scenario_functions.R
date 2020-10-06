test_that("local_outbreak works",{

  df <- list(id = sample(1:100, 10, replace = FALSE),
             area = rep("ar1", 10),
             current_risk = runif(10, 0, 1),
             beta0 = rep(0, 10),
             betaxs = rep(0, 10),
             hid_status = rep(0, 10),
             presymp_days = sample(1:10, size = 10, replace = TRUE),
             symp_days = sample(5:15, size = 10, replace = TRUE),
             status = c(rep(0,5), 1, 2, 3, 4, 0),
             new_status = c(rep(0,5), 1, 2, 3, 4, 0))

  expect_equal(local_outbreak(df = df, msoa_infect="ar1")[["id"]], df$id)  

})
