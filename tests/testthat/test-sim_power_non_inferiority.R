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

test_that("true_diff defaults to 0 (no true difference between groups)", {
  set.seed(1)
  res_default <- sim_power_ni_normal(
    nsim = 200, npergroup = 100, ntest = 1,
    ni_limit = log10(2 / 3), test_req = 1, test_opt = 0,
    sd = 0.4, t_level = 0.95
  )
  set.seed(1)
  res_explicit <- sim_power_ni_normal(
    nsim = 200, npergroup = 100, ntest = 1,
    ni_limit = log10(2 / 3), test_req = 1, test_opt = 0,
    sd = 0.4, true_diff = 0, t_level = 0.95
  )

  expect_equal(res_default$power, res_explicit$power)
})

test_that("power decreases as true_diff worsens toward the NI margin", {
  set.seed(123)
  power_at <- function(td) {
    sim_power_ni_normal(
      nsim = 1000, npergroup = 250, ntest = 1,
      ni_limit = log10(2 / 3), test_req = 1, test_opt = 0,
      sd = 0.4, true_diff = td, t_level = 0.95
    )$power
  }

  p0 <- power_at(0)
  p1 <- power_at(-0.1)
  p2 <- power_at(-0.176)

  expect_true(p0 > p1)
  expect_true(p1 > p2)
})

test_that("true_diff accepts a vector of length ntest", {
  res <- sim_power_ni_normal(
    nsim = 50, npergroup = 30, ntest = 2,
    ni_limit = c(-0.2, -0.2), test_req = 1, test_opt = 1,
    sd = c(0.3, 0.4), true_diff = c(0, -0.05), corr = 0.2,
    t_level = c(0.95, 0.9)
  )

  expect_s3_class(res, "empirical_power_result")
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

  expect_error(sim_power_ni_normal(
    nsim = 100,
    npergroup = 50,
    ntest = 3,
    ni_limit = log10(2 / 3),
    test_req = 1,
    test_opt = 1,
    sd = 0.4,
    true_diff = c(0, -0.05),
    t_level = 0.95
  ), regexp = "Length of true_diff is incorrect")
})

