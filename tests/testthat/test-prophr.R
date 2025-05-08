test_that("prophr returns correct values", {
  # Basic case
  expect_equal(prophr(0.05, 1), 0.05)

  # Hazard ratio < 1 should reduce event probability
  expect_lt(prophr(0.10, 0.5), 0.10)

  # Hazard ratio > 1 should increase event probability
  expect_gt(prophr(0.10, 1.5), 0.10)

  # Edge case: p0 = 0
  expect_equal(prophr(0, 0.5), 0)

  # Edge case: p0 = 1
  expect_equal(prophr(1, 0.5), 1)

  # Vectorized input is  supported
  expect_error(prophr(c(0.1, 0.2), 0.5),
               "p0 and hr must be of the same length")

  # Invalid inputs
  expect_error(prophr(-0.1, 0.5))
  expect_error(prophr(1.1, 0.5))
  expect_error(prophr(0.1, -0.5))
})
