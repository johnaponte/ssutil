test_that("Returns correct structure and values", {
  res <- sim_power_ni_normal(
    nsimul = 100,
    npergroup = 50,
    ntest = 3,
    ni_limit = log10(2 / 3),
    test_req = 1,
    test_opt = 1,
    sd = 0.4,
    corr = 0,
    conf.level = 0.95
  )

  expect_s3_class(res, "data.frame")
  expect_named(res, c("power", "conf.low", "conf.high"))
  expect_true(all(res$power >= 0 & res$power <= 1))
})

test_that("Handles vector inputs for sd and conf.level", {
  res <- sim_power_ni_normal(
    nsimul = 50,
    npergroup = 30,
    ntest = 2,
    ni_limit = c(-0.2, -0.2),
    test_req = 1,
    test_opt = 1,
    sd = c(0.3, 0.4),
    corr = 0.2,
    conf.level = c(0.95, 0.9)
  )

  expect_s3_class(res, "data.frame")
  expect_named(res, c("power", "conf.low", "conf.high"))
})

test_that("Handles correlation vector correctly", {
  res <- sim_power_ni_normal(
    nsimul = 30,
    npergroup = 20,
    ntest = 3,
    ni_limit = -0.2,
    test_req = 2,
    test_opt = 1,
    sd = 0.4,
    corr = c(0.2, 0.3, 0.4),  # corresponds to (1,2), (1,3), (2,3)
    conf.level = 0.95
  )

  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 3)
})

test_that("Throws errors for incorrect inputs", {
  expect_error(sim_power_ni_normal(
    nsimul = 100,
    npergroup = 50,
    ntest = 3,
    ni_limit = log10(2 / 3),
    test_req = 3,
    test_opt = 2,
    sd = 0.4,
    conf.level = 0.95
  ), regexp = "test_req")

  expect_error(sim_power_ni_normal(
    nsimul = 100,
    npergroup = 50,
    ntest = 3,
    ni_limit = log10(2 / 3),
    test_req = 1,
    test_opt = 1,
    sd = c(0.3, 0.4),
    corr = c(0.2),
    conf.level = 0.95
  ), regexp = "Length of sd is incorrect|Incorrect number of correlations")
})
