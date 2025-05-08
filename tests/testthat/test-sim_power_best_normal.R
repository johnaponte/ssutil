test_that("Function returns expected structure and types", {
  result <- sim_power_best_normal(
    noutcomes = 2,
    sd = c(1, 1),
    dif = c(0.2, 0.3),
    ngroups = 3,
    npergroup = c(30, 25, 25),
    nsimul = 100
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("power", "conf.low", "conf.high", "nsim"))
  expect_true(is.numeric(result$power))
  expect_true(is.numeric(result$conf.low))
  expect_true(is.numeric(result$conf.high))
  expect_equal(result$nsim, 100)
})

test_that("Function handles scalar inputs correctly", {
  result <- sim_power_best_normal(
    noutcomes = 1,
    sd = 1,
    dif = 0.2,
    ngroups = 2,
    npergroup = 20,
    nsimul = 50
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$nsim, 50)
})

test_that("Errors are thrown for invalid inputs", {
  expect_error(sim_power_best_normal(1, 1, -0.1, 2, 10, 10), "Dif should be positive")
  expect_error(sim_power_best_normal(1, 1, 0.1, 1, 10, 10), "ngroups must be at least 2")
  expect_error(sim_power_best_normal(1, 1, 0.1, 2, 0, 10), "All npergroup values must be >= 1")
  expect_error(sim_power_best_normal(1, 1, 0.1, 3, c(10, 10), 10))
})
