test_that("sim_power_best_bin_rank returns expected structure and values", {
  res <- sim_power_best_bin_rank(
    noutcomes = 1,
    p1 = 0.7,
    dif = 0.2,
    weights = 1,
    ngroups = 3,
    npergroup = 30,
    nsim = 100
  )

  expect_s3_class(res, "empirical_power_result")
  expect_true(all(c("power", "conf.low", "conf.high", "nsim") %in% names(res)))
  expect_equal(res$nsim, 100)
  expect_true(res$power >= 0 && res$power <= 1)
})


test_that("sim_power_best_bin_rank handles input validation correctly", {
  expect_error(
    sim_power_best_bin_rank(1, 0.2, 0.3, 1, 3, 30, 100),
    "p1 - dif must be between 0 and 1"
  )

  expect_error(
    sim_power_best_bin_rank(1, 0.7, 0.2, 1, 3, c(30, 30), 100),
    "Incorrect length of npergroup"
  )

  expect_error(
    sim_power_best_bin_rank(2, 0.7, 0.2, c(1, 2, 3), 3, 30, 100),
    "Invalid length of weights"
  )
})



