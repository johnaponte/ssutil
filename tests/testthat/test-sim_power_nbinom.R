test_that("sim_power_nbinom returns valid structure and values", {
  result <- sim_power_nbinom(
    n1 = 50, n2 = 50,
    ir1 = 0.5,
    tm = 1.5,
    rr = 0.7,
    boundary = 1,
    dispersion = 2,
    alpha = 0.05,
    nsim = 100
  )

  expect_s3_class(result, "empirical_power_result")
  expect_named(result, c("power", "conf.low", "conf.high", "conf.level", "nsim"))
})

test_that("Function throws error for invalid inputs", {
  expect_error(sim_power_nbinom(
    n1 = 0, n2 = 50,
    ir1 = 0.5,
    tm = 1.5,
    rr = 0.7,
    boundary = 1,
    dispersion = 2,
    alpha = 0.05,
    nsim = 100
  ), regexp = "n1 > 0")

  expect_error(sim_power_nbinom(
    n1 = 50, n2 = 50,
    ir1 = -0.5,
    tm = 1.5,
    rr = 0.7,
    boundary = 1,
    dispersion = 2,
    alpha = 0.05,
    nsim = 100
  ), regexp = "ir1 > 0")
})

test_that("Power is close to 1 when difference is large and sample size is high", {
  result <- sim_power_nbinom(
    n1 = 150, n2 = 150,
    ir1 = 0.5,
    tm = 1.5,
    rr = 0.3,  # large effect
    boundary = 0.8,
    dispersion = 1,
    alpha = 0.05,
    nsim = 100
  )

  expect_gt(result$power, 0.9)
})
