test_that("sim_power_best_binomial returns expected output", {
  res <- sim_power_best_binomial(
    noutcomes = 1,
    p1 = 0.7,
    dif = 0.2,
    ngroups = 3,
    npergroup = 30,
    nsim = 100
  )

  expect_s3_class(res, "empirical_power_result")
  expect_true(all(c("power", "conf.low", "conf.high", "nsim","conf.level") %in% names(res)))
  expect_equal(res$nsim, 100)
  expect_true(res$power >= 0 && res$power <= 1)
})


test_that("sim_power_best_binomial input validation works", {
  # Invalid difference
  expect_error(
    sim_power_best_binomial(1, 0.2, 0.3, 3, 30, 100),
    "p1 - dif"
  )

  # Wrong npergroup length
  expect_error(
    sim_power_best_binomial(1, 0.7, 0.2, 3, c(30, 30), 100))

  # Invalid p1 range
  expect_error(
    sim_power_best_binomial(1, -0.1, 0.2, 3, 30, 100))
})


