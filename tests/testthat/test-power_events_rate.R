test_that("power_events_rate returns correct structure and values", {
  result <- power_events_rate(30, 0.1, 1)
  expect_s3_class(result, "power_events_rate")
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("N", "Risk", "≥1"))
  expect_equal(result[1, "N"], c(N = 30))
  expect_equal(result[1, "Risk"], c(Risk = 0.1))
  expect_equal(result[1, "≥1"], c("≥1" = 0.9576088), tolerance = 1e-5)

  # Multiple event thresholds
  res_e <- power_events_rate(30, 0.1, c(1, 2))
  expect_equal(colnames(res_e), c("N", "Risk", "≥1", "≥2"))
  expect_equal(res_e[1, "≥2"], c("≥2" = 0.816305), tolerance = 1e-5)

  # Grid over n and r: N varies slowest, Risk fastest
  res_grid <- power_events_rate(c(30, 60), c(0.05, 0.1), c(1, 2))
  expect_equal(nrow(res_grid), 4)
  expect_equal(unname(res_grid[, "N"]), c(30, 30, 60, 60))
  expect_equal(unname(res_grid[, "Risk"]), c(0.05, 0.1, 0.05, 0.1))
  expect_equal(
    unname(res_grid[, "≥1"]),
    c(0.7853612, 0.9576088, 0.9539302, 0.998203),
    tolerance = 1e-5
  )
  expect_equal(
    unname(res_grid[, "≥2"]),
    c(0.4464579, 0.816305, 0.8084466, 0.9862229),
    tolerance = 1e-5
  )

  # Invalid inputs
  expect_error(power_events_rate(30, 1.5, 1))
  expect_error(power_events_rate(30, -0.1, 1))
  expect_error(power_events_rate(0, 0.1, 1))
  expect_error(power_events_rate(30, 0.1, 0))
  expect_error(power_events_rate(30, 0.1, 1.5))
})

test_that("format.power_events_rate returns formatted text", {
  result <- power_events_rate(30, 0.1, 1)
  output <- format(result)
  expect_true(is.character(output))
  expect_match(output, "N")
  expect_match(output, "≥1")
  expect_match(output, "1/10")
  expect_match(output, "95.8%")

  output_digits <- format(result, digits = 3)
  expect_match(output_digits, "95.761%")
})

test_that("print.power_events_rate prints without error", {
  result <- power_events_rate(30, 0.1, 1)
  expect_invisible(print(result))
})
