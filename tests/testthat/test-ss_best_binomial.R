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
  # There are several approximations in the tables. The value to test is in
  # agreement with the results from the table
  expect_equal(ss_best_binomial(0.90, 0.50, 0.10, 2),  82)  # 83
  expect_equal(ss_best_binomial(0.90, 0.50, 0.25, 2),  13)  # 14
  expect_equal(ss_best_binomial(0.90, 0.50, 0.10, 3),  123) # 125
  expect_equal(ss_best_binomial(0.90, 0.50, 0.25, 3),  19)  # 20
  expect_equal(ss_best_binomial(0.90, 0.50, 0.10, 4),  149) # 150
  expect_equal(ss_best_binomial(0.90, 0.50, 0.25, 4),  22)  # 24
  expect_equal(ss_best_binomial(0.90, 0.50, 0.10, 10), 219) # 222
  expect_equal(ss_best_binomial(0.90, 0.50, 0.25, 10), 32)  # 32
})

test_that("power_best_binomial throws errors for invalid inputs", {
  expect_error(power_best_binomial(p1 = -0.1, dif = 0.1, ngroups = 3, npergroup = 20))
  expect_error(power_best_binomial(p1 = 0.8, dif = 0.9, ngroups = 3, npergroup = 20))
  expect_error(power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 1, npergroup = 20))
  expect_error(power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 3.5, npergroup = 20))
  expect_error(power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 3, npergroup = 0))
})

test_that("ss_best_binomial returns valid integer and satisfies power", {
   
    # using a fixed set of values, change K 
    # Common simulation parameters
    p1_val   <- 0.8
    d_val    <- 0.1
    n_val    <- 30
    nsim_val <- 100000
    seed_val <- 1
    
    for (k in c(2,3,4, 5, 10)) {
      
      # Monte Carlo reference (ground truth) with 95% CI
      set.seed(1234)
      mc <- sim_power_best_binomial(
        noutcomes = 1,
        p1 = p1_val, d = d_val, ngroups = k,
        npergroup = n_val, nsim = nsim_val
      )
      
      # Value under test
      achieved_power <- power_best_binomial(
        p1 = p1_val, dif = d_val, ngroups = k, npergroup = n_val
      )
      
      expect_gte(achieved_power, mc$conf.low)
      expect_lte(achieved_power, mc$conf.high)
    }
})

test_that("ss_best_binomial stops if max_n is exceeded", {
  expect_error(
    ss_best_binomial(power = 0.99, p1 = 0.6, dif = 0.1, ngroups = 4, max_n = 5),
    regexp = "max_n limit reached"
  )
})
