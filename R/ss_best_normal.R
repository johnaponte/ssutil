#' Sample Size for Selecting the Best Treatment in a Single-Factor Normal Response Using the Indifference-Zone Approach
#'
#' Estimate the minimum common sample size per group required to guarantee the
#' probability (power) of correctly selecting the best group
#' (i.e., the group with the highest mean) using the indifference-zone approach.
#'
#' The procedure assumes a normal distribution of responses, with a common known
#' variance. 'delta' is the smallest difference considered worth detecting between
#' the best group and the next least favorable group.
#'
#' Reference: Bechhofer, R.E., Santner, T.J., & Goldsman, D.M. (1995).
#' Design and Analysis of Experiments for Statistical Selection,
#' Screening, and Multiple Comparisons.
#' Wiley Series in Probability and Statistics. (Procedure Nb)
#'
#' @param groups number of groups in the test.
#' @param delta change to be detected.
#' @param sd  common standard deviation of the response variable.
#' @param power desired power of the test.
#' @param seed the seed used for the multz procedure
#'
#' @return Returns the sample size needed for each group to achieve the desired power.
#' @examples
#' ss_best_normal(groups = 3, delta = 0.5, sd = 1, power = 0.8)
#'
#' @export
ss_best_normal <- function(groups, delta, sd, power, seed=NULL) {
  ceiling(2*(sd*multz(1-power,groups-1,0.5, seed)/delta)^2)
}


