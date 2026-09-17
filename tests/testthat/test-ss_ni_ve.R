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
  expect_error(ss_ni_ve(ve_lci = 0.95, ve_exp = 0.6), regexp = "ve_ac must be provided when ve_exp is specified")
  expect_error(ss_ni_ve(ve_lci = 0.95, ve_exp = 1.2, ve_ac = 0.6), regexp = "ve_exp should be a value between 0 and 1")
  expect_error(ss_ni_ve(ve_lci = 0.95, ve_exp = 0.6, ve_ac = -0.1), regexp = "ve_ac should be a value between 0 and 1")
})

test_that("ss_ni_ve with ve_exp defaults to the same result as without it (theta = 1)", {
  res1 <- ss_ni_ve(ve_lci = 0.95)
  res2 <- ss_ni_ve(ve_lci = 0.95, ve_exp = 0.5, ve_ac = 0.5)

  expect_equal(res1[["Total number of events"]], res2[["Total number of events"]])
})

# Fleming does not explain how they obtain their values but there are some
# rough approximations in the maths. For example 1/sqrt(0.0855) is 3.420 but
# is presented as 3.421. alpha here is one-sided, 0.025, matching the paper's
# stated "preserving a 2.5% false positive error rate" (not 0.05). The event
# counts are reproduced via Freedman's (1982) log-rank formula to within 2
# events, likely due to Fleming using rounded z critical values.
test_that("ss_ni_ve produce similar values to Table 1 of Fleming et al)",{
  res <- ss_ni_ve(
            ve_lci = (1-0.0855),
            power = 0.9,
            alpha = 0.025,
            use70 = FALSE,
            preserve = 0.5)

    expect_lte(abs(res[[5]] - 34), 2)
    expect_equal(res[[2]], 3.421, tolerance = 0.001)
})

test_that("ss_ni_ve produce similar values to Table 1 of Fleming et al (2))",{
  res <- ss_ni_ve(
    ve_lci = (1-0.4997),
    power = 0.9,
    alpha = 0.025,
    use70 = FALSE,
    preserve = 0.5)

  expect_lte(abs(res[[5]] - 355), 2)
  expect_equal(res[[2]], 1.415, tolerance = 0.001)
})

# Table 2 of Fleming et al. assumes the experimental vaccine (EXP) has a fixed
# 60% efficacy versus placebo, regardless of the active comparator's (AC) own
# efficacy, via ve_exp/ve_ac. The margin (delta0, via use70 = TRUE) matches to
# 3 decimals in all three rows below (AC at 70%, 60%, and 50% VE, i.e. the true
# EXP/AC hazard ratio theta above, at, and below 1). The event counts match
# less tightly than Table 1 (off by 2 to 12 events, larger when theta is
# farther from 1), for the same reason as Table 1: Fleming's underlying
# z-value rounding.
test_that("ss_ni_ve produce similar values to Table 2 of Fleming et al (AC 60% VE)", {
  res <- ss_ni_ve(
    ve_lci = 1 - 0.4997,
    power = 0.9,
    use70 = TRUE,
    ve_exp = 0.60,
    ve_ac = 0.60)

  expect_equal(res[[2]], 1.674, tolerance = 0.001)
  expect_lte(abs(res[[5]] - 164), 2)
})

test_that("ss_ni_ve produce similar values to Table 2 of Fleming et al (AC 70% VE)", {
  res <- ss_ni_ve(
    ve_lci = 1 - 0.3781,
    power = 0.9,
    use70 = TRUE,
    ve_exp = 0.60,
    ve_ac = 0.70)

  expect_equal(res[[2]], 2.213, tolerance = 0.001)
  expect_lte(abs(res[[5]] - 180), 12)
})

test_that("ss_ni_ve produce similar values to Table 2 of Fleming et al (AC 50% VE)", {
  res <- ss_ni_ve(
    ve_lci = 1 - 0.6216,
    power = 0.9,
    use70 = TRUE,
    ve_exp = 0.60,
    ve_ac = 0.50)

  expect_equal(res[[2]], 1.346, tolerance = 0.001)
  expect_lte(abs(res[[5]] - 158), 5)
})
