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


test_that("least favourable binomial rannk returns correct structure", {
  res <- lf_config_bin_rank(
    noutcomes = 1,
    dif = 0.2,
    weights = 1,
    ngroups = 3,
    npergroup = 30,
    nsim = 100,
    p1 = seq(0.3, 0.7, length.out = 5)
  )

  expect_s3_class(res, "lf_config_bin_rank")
  expect_true(is.numeric(res$minprob))
  expect_true(is.numeric(res$minpow))
  expect_s3_class(res$simulation, "data.frame")
})


test_that("plot function works for lf_config_bin_rank", {
  res <- lf_config_bin_rank(
    noutcomes = 1,
    dif = 0.2,
    weights = 1,
    ngroups = 3,
    npergroup = 30,
    nsim = 100,
    p1 = seq(0.3, 0.7, length.out = 5)
  )

  plot <- ggplot_lf_config_bin_rank(res)
  expect_s3_class(plot, "ggplot")
})


test_that("format and print methods for lf_config_bin_rank", {
  res <- lf_config_bin_rank(
    noutcomes = 1,
    dif = 0.2,
    weights = 1,
    ngroups = 3,
    npergroup = 30,
    nsim = 100,
    p1 = seq(0.3, 0.7, length.out = 5)
  )

  formatted <- format(res)
  expect_type(formatted, "character")

  expect_invisible(print(res))
})

