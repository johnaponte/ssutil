
test_that("sim_power_best_norm_rank returns expected output", {
  res <- sim_power_best_norm_rank(
    noutcomes = 2,
    sd = 1,
    dif = 0.2,
    weights = 1,
    ngroups = 3,
    npergroup = 30,
    nsimul = 50  #low for test
  )

  expect_s3_class(res, "data.frame")
  expect_true(all(c("power", "conf.low", "conf.high", "nsim") %in% names(res)))
  expect_equal(res$nsim, 50)
  expect_true(res$power >= 0 && res$power <= 1)
})


test_that("sim_power_best_norm_rank handles unequal group sizes", {
  res <- sim_power_best_norm_rank(
    noutcomes = 1,
    sd = 1,
    dif = 0.2,
    weights = 1,
    ngroups = 3,
    npergroup = c(30, 25, 20),
    nsimul = 20
  )

  expect_s3_class(res, "data.frame")
  expect_equal(res$nsim, 20)
})


test_that("sim_power_best_norm_rank handles vector inputs correctly", {
  res <- sim_power_best_norm_rank(
    noutcomes = 3,
    sd = c(1, 0.9, 1.1),
    dif = c(0.2, 0.15, 0.25),
    weights = c(0.5, 0.3, 0.2),
    ngroups = 3,
    npergroup = 30,
    nsimul = 30
  )

  expect_s3_class(res, "data.frame")
  expect_true(res$power >= 0 && res$power <= 1)
})


test_that("sim_power_best_norm_rank fails on mismatched input lengths", {
  expect_error(sim_power_best_norm_rank(
    noutcomes = 2,
    sd = c(1, 0.9),
    dif = 0.2,
    weights = c(1, 1, 1),  # too long
    ngroups = 3,
    npergroup = 30,
    nsimul = 10
  ), "length.*weights")
})

