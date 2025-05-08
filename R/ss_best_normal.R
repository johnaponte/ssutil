#' Sample Size for Selecting the Best Treatment in a Normal Response (Indifference-Zone)
#'
#' Calculates the minimum common sample size per group needed to achieve a specified
#' probability (power) of correctly selecting the best group using the
#' indifference-zone approach. This method assumes normally distributed responses with
#' a known and common standard deviation.
#'
#' The indifference-zone approach guarantees that the probability of correct selection
#' is at least \code{power}, assuming the best group's mean exceeds the others by at
#' least \code{delta}. The calculation is based on Bechhofer's Procedure Nb.
#'
#' @param groups Integer. Number of groups (treatments) being compared.
#' @param delta Numeric. Minimum difference in means between the best group and the second-best that is considered meaningful.
#' @param sd Numeric. Common standard deviation of the response variable.
#' @param power Numeric. Desired probability of correctly selecting the best group (must be between 0 and 1).
#' @param seed Optional. Integer seed to use in the internal call to \code{multz()}.
#'
#' @return Integer. Sample size required per group to achieve the specified power.
#'
#' @note
#' The function uses the quantile function \code{multz()}, which computes critical values
#' for the selection procedure. This implementation assumes equal variances and independent samples.
#'
#' @references
#' Bechhofer, R.E., Santner, T.J., & Goldsman, D.M. (1995).
#' *Design and Analysis of Experiments for Statistical Selection, Screening, and Multiple Comparisons.*
#' Wiley Series in Probability and Statistics. ISBN: 0-471-57427-9.
#'
#' @examples
#' ss_best_normal(groups = 3, delta = 0.5, sd = 1, power = 0.8)
#'
#' @export
ss_best_normal <- function(groups, delta, sd, power, seed = NULL) {

  stopifnot("groups must be >=2 0" = groups >= 2)
  stopifnot("delta must be > 0" = delta > 0)
  stopifnot("sd must be > 0" = sd > 0)
  stopifnot("power must be > 0 and < 1" = power > 0 && power < 1)
  if (!is.null(seed)) stopifnot("seed must be > 0" = seed > 0)

  ceiling(2 * (sd * multz(1 - power, groups - 1, 0.5, seed) / delta)^2)
}
