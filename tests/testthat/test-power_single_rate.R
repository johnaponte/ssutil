test_that("power_single_rate returns correct structure and values", {
  # Basic case
  result <- power_single_rate(30, 0.9)
  expect_s3_class(result, "power_single_rate")
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("n", "power", "proportion"))
  expect_equal((result[1, "n"]), c(n=30))
  expect_equal(result[1, "power"], c(power=0.9))
  expect_true(result[1, "proportion"] > 0)
  expect_true(result[1, "proportion"] < 1)
  expect_equal(result[1, "proportion"] , 0.07391014, ignore_attr=TRUE, tolerance = 1e-5)

  # Vector input
  res_vec <- power_single_rate(c(30, 60), c(0.8, 0.9))
  expect_equal(nrow(res_vec), 4)

  # Edge cases
  expect_error(power_single_rate(30, 1.5))
  expect_error(power_single_rate(30, -0.1))
  expect_error(power_single_rate(0, 0.9))
})

test_that("format.power_single_rate returns formatted text", {
  result <- power_single_rate(30, 0.9)
  output <- format(result)
  expect_true(is.character(output))
  expect_match(output, "A study with")
})

test_that("print.power_single_rate prints without error", {
  result <- power_single_rate(30, 0.9)
  expect_invisible(print(result))
})
