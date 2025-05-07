# Define your expectations from multz function
# Expected values from table B.1 and B.2 from  Bechhofer, Design and analysis of
# experiments for statistical selection, screening and multiple comparisons book

expect_multz <- function(alpha, n, rho, expected_result, tol){
  output <- multz(alpha, n, rho)

  # Compare the result with an expected value by using the expect_equal function
  expect_equal(output, expected_result, tolerance = tol)
}

# Write tests
test_that("multz function calculates correct z-values", {
  expect_multz(0.01, 1, 1, 2.326, 0.001)
  expect_multz(0.1, 3, 0.5, 1.734, 0.001)
  expect_multz(0.2, 7, 0.5, 1.655, 0.001)
  expect_multz(0.1, 7, 0.1, 2.16, 0.01)
  expect_multz(0.01, 24, 0.1, 3.34, 0.01)
  expect_multz(0.1, 7, 0.3, 2.11, 0.01)
  expect_multz(0.01, 24, 0.3, 3.32, 0.01)
  expect_multz(0.1, 7, 0.7, 1.90, 0.01)
  expect_multz(0.01, 24, 0.7, 3.13, 0.01)

})

