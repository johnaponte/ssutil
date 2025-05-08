test_that("Function returns expected structure and types", {
  result <- sim_power_equivalence_normal(
    ngroups = 3,
    npergroup = 30,
    sd = 0.5,
    llimit = log10(2/3),
    ulimit = log10(3/2),
    nsimul = 100,
    conf.level = 0.95
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("power", "conf.low", "conf.high", "nsim"))
  expect_true(is.numeric(result$power))
  expect_true(is.numeric(result$conf.low))
  expect_true(is.numeric(result$conf.high))
  expect_equal(result$nsim, 100)
})

test_that("Function detects equivalence when variability is low", {
  result <- sim_power_equivalence_normal(
    ngroups = 2,
    npergroup = 100,
    sd = 0.1,
    llimit = -0.2,
    ulimit = 0.2,
    nsimul = 100,
    conf.level = 0.95
  )

  expect_gt(result$power, 0.9)
})

test_that("Errors are raised for invalid inputs", {
  expect_error(sim_power_equivalence_normal(
    ngroups = 1,
    npergroup = 30,
    sd = 0.4,
    llimit = -0.1,
    ulimit = 0.1,
    nsimul = 10
  ), regexp = "ngroups")

  expect_error(sim_power_equivalence_normal(
    ngroups = 3,
    npergroup = 0,
    sd = 0.4,
    llimit = -0.1,
    ulimit = 0.1,
    nsimul = 10
  ), regexp = "npergroup")
})
