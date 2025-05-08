test_that("ss_ni_ve returns expected structure and valid values", {
  res <- ss_ni_ve(ve_lci = 0.95)

  expect_type(res, "list")
  expect_named(res, c(
    "Upper limit of the HR used to estimate the sample size",
    "Non-inferior margin in HR scale",
    "Alpha",
    "Power",
    "Total number of events",
    "Max HR to declare NI",
    "Max number of events in the experimental group",
    "Non-inferior criteria"
  ))

  expect_true(res[["Alpha"]] > 0 && res[["Alpha"]] < 1)
  expect_true(res[["Power"]] > 0 && res[["Power"]] < 1)
  expect_true(res[["Total number of events"]] > 0)
  expect_true(res[["Non-inferior margin in HR scale"]] > 0)
})

test_that("ss_ni_ve changes with use70 = TRUE", {
  res1 <- ss_ni_ve(ve_lci = 0.95, use70 = FALSE)
  res2 <- ss_ni_ve(ve_lci = 0.95, use70 = TRUE)

  expect_true(res1[["Non-inferior margin in HR scale"]] != res2[["Non-inferior margin in HR scale"]])
  expect_match(res2[["Non-inferior criteria"]], "30%")
})

test_that("ss_ni_ve reacts to different preserve values", {
  res1 <- ss_ni_ve(ve_lci = 0.95, preserve = 0.4)
  res2 <- ss_ni_ve(ve_lci = 0.95, preserve = 0.7)

  expect_true(res1[["Non-inferior margin in HR scale"]] != res2[["Non-inferior margin in HR scale"]])
})

test_that("ss_ni_ve throws errors for invalid inputs", {
  expect_error(ss_ni_ve(ve_lci = -0.1), regexp = "ve_lci should be a value between 0 and 1")
  expect_error(ss_ni_ve(ve_lci = 0.95, alpha = 1.2), regexp = "alpha should be a value between 0 and 1")
  expect_error(ss_ni_ve(ve_lci = 0.95, power = 0), regexp = "power should be a value between 0 and 1")
  expect_error(ss_ni_ve(ve_lci = 0.95, preserve = -0.1), regexp = "Preserve should be between 0 and 1")
  expect_error(ss_ni_ve(ve_lci = c(0.9, 0.95)), regexp = "ve_lci should be a single number")
})

# Fleming does not explain how they obtain their values but there are some
# rough approximations in the maths. For example 1/sqrt(0.0855) is 3.420 but
# is presented as 3.421
test_that("ss_ni_ve produce similar values to Table 1 of Fleming et al)",{
  res <- ss_ni_ve(
            ve_lci = (1-0.0855),
            power = 0.9,
            alpha = 0.05,
            use70 = FALSE,
            preserve = 0.5)

    expect_equal(res[[5]], 34, tolerance = 1)
    expect_equal(res[[2]], 3.421, tolerance = 0.001)
})

test_that("ss_ni_ve produce similar values to Table 1 of Fleming et al (2))",{
  res <- ss_ni_ve(
    ve_lci = (1-0.4997),
    power = 0.9,
    alpha = 0.05,
    use70 = FALSE,
    preserve = 0.5)

  expect_equal(res[[5]], 355, tolerance = 1)
  expect_equal(res[[2]], 1.415, tolerance = 0.001)
})
