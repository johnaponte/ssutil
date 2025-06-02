test_that("Returns correct structure and values", {
  res <- sim_power_ni_normal(
    nsim = 100,
    npergroup = 50,
    ntest = 3,
    ni_limit = log10(2 / 3),
    test_req = 1,
    test_opt = 1,
    sd = 0.4,
    corr = 0,
    t_level = 0.95
  )

  expect_s3_class(res, "empirical_power_result")
  expect_named(res, c("power", "conf.low", "conf.high","conf.level","nsim"))
  expect_true(all(res$power >= 0 & res$power <= 1))
})

test_that("Handles vector inputs for sd and t_level", {
  res <- sim_power_ni_normal(
    nsim = 50,
    npergroup = 30,
    ntest = 2,
    ni_limit = c(-0.2, -0.2),
    test_req = 1,
    test_opt = 1,
    sd = c(0.3, 0.4),
    corr = 0.2,
    t_level = c(0.95, 0.9)
  )

  expect_s3_class(res, "empirical_power_result")
  expect_named(res, c("power", "conf.low", "conf.high","conf.level","nsim"))
})

test_that("Handles correlation vector correctly", {
  res <- sim_power_ni_normal(
    nsim = 30,
    npergroup = 20,
    ntest = 3,
    ni_limit = -0.2,
    test_req = 2,
    test_opt = 1,
    sd = 0.4,
    corr = c(0.2, 0.3, 0.4),  # corresponds to (1,2), (1,3), (2,3)
    t_level = 0.95
  )

  expect_s3_class(res, "empirical_power_result")
  expect_named(res, c("power", "conf.low", "conf.high","conf.level","nsim"))
})

test_that("Throws errors for incorrect inputs", {
  expect_error(sim_power_ni_normal(
    nsim = 100,
    npergroup = 50,
    ntest = 3,
    ni_limit = log10(2 / 3),
    test_req = 3,
    test_opt = 2,
    sd = 0.4,
    t_level = 0.95
  ), regexp = "test_req")

  expect_error(sim_power_ni_normal(
    nsim = 100,
    npergroup = 50,
    ntest = 3,
    ni_limit = log10(2 / 3),
    test_req = 1,
    test_opt = 1,
    sd = c(0.3, 0.4),
    corr = c(0.2),
    t_level = 0.95
  ), regexp = "Length of sd is incorrect|Incorrect number of correlations")
})

