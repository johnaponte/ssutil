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


test_that("lf_config_binomial returns correct structure and values", {
  res <- lf_config_binomial(
    dif = 0.2,
    ngroups = 3,
    npergroup = 30,
    nsim = 100,
    prob = seq(0.3, 0.7, length.out = 5)
  )

  expect_s3_class(res, "lf_config_binomial")
  expect_type(res$minprob, "double")
  expect_s3_class(res$simulation, "data.frame")
  expect_true("pred" %in% names(res$simulation))
})


test_that("format and print methods for lf_config_binomial work", {
  res <- lf_config_binomial(
    dif = 0.2,
    ngroups = 3,
    npergroup = 30,
    nsim = 100,
    prob = seq(0.3, 0.7, length.out = 5)
  )

  formatted <- format(res)
  expect_type(formatted, "character")

  expect_invisible(print(res))
})


test_that("ggplot_lf_config_binomial returns a ggplot object", {
  res <- lf_config_binomial(
    dif = 0.2,
    ngroups = 3,
    npergroup = 30,
    nsim = 100,
    prob = seq(0.3, 0.7, length.out = 5))

  plot <- ggplot_lf_config_binomial(res)
  expect_s3_class(plot, "ggplot")
})

