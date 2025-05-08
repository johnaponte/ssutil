test_that("power_best_binomial returns valid probability between 0 and 1", {
  p <- power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 4, npergroup = 50)
  expect_type(p, "double")
  expect_true(p >= 0 && p <= 1)
})

test_that("power increases with npergroup", {
  p_small <- power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 4, npergroup = 30)
  p_large <- power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 4, npergroup = 60)
  expect_lt(p_small, p_large)
})

test_that("Values agree with Table 1 to 4 of Sobel paper",{
  expect_equal(ss_best_binomial(0.90,0.50,0.10,2),83, tolerance = 1)
  expect_equal(ss_best_binomial(0.90,0.50,0.25,2),14, tolerance = 1)
  expect_equal(ss_best_binomial(0.90,0.50,0.10,3),125, tolerance = 1)
  expect_equal(ss_best_binomial(0.90,0.50,0.25,3),20, tolerance = 1)
  expect_equal(ss_best_binomial(0.90,0.50,0.10,4),150, tolerance = 1)
  expect_equal(ss_best_binomial(0.90,0.50,0.25,4),29, tolerance = 1)
  expect_equal(ss_best_binomial(0.90,0.50,0.10,10),222, tolerance = 1)
  expect_equal(ss_best_binomial(0.90,0.50,0.25,10),35, tolerance = 1)
})

test_that("power_best_binomial throws errors for invalid inputs", {
  expect_error(power_best_binomial(p1 = -0.1, dif = 0.1, ngroups = 3, npergroup = 20))
  expect_error(power_best_binomial(p1 = 0.8, dif = 0.9, ngroups = 3, npergroup = 20))
  expect_error(power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 1, npergroup = 20))
  expect_error(power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 3.5, npergroup = 20))
  expect_error(power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 3, npergroup = 0))
})

test_that("ss_best_binomial returns valid integer and satisfies power", {
  n <- ss_best_binomial(power = 0.8, p1 = 0.8, dif = 0.2, ngroups = 4)
  expect_type(n, "double")
  expect_true(n >= 1)

  achieved_power <- power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 4, npergroup = n)
  expect_gte(achieved_power, 0.8)
})

test_that("ss_best_binomial stops if max_n is exceeded", {
  expect_error(
    ss_best_binomial(power = 0.99, p1 = 0.6, dif = 0.1, ngroups = 4, max_n = 5),
    regexp = "max_n limit reached"
  )
})
