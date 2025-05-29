#' Sample Size for Selecting the Best Treatment in a Normal Response (Indifference-Zone)
#'
#' Calculates the minimum common sample size per group needed to achieve a specified
#' probability (power) of correctly selecting the best group using the
#' indifference-zone approach. This method assumes normally distributed responses with
#' a known and common standard deviation.
#'
#' The indifference-zone approach guarantees that the probability of correct selection
#' is at least \code{power}, assuming the best group's mean exceeds the others by at
#' least \code{dif}. The calculation is based on Bechhofer's Procedure Nb.
#'
#' @param power Numeric. Desired probability of correctly selecting the best group.
#' @param dif Numeric. Indifferent-zone. Minimum difference that is considered meaningful.
#' @param sd Numeric. Common standard deviation of the response variable.
#' @param ngroups Integer. Number of groups (treatments) being compared.
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
#' ss_best_normal( power = 0.8, dif = 0.5, sd = 1, ngroups = 3)
#'
#' @export
ss_best_normal <- function(power, dif, sd, ngroups, seed = NULL) {

  stopifnot("ngroups must be >=2 0" = ngroups >= 2)
  stopifnot("dif must be > 0" = dif > 0)
  stopifnot("sd must be > 0" = sd > 0)
  stopifnot("power must be > 0 and < 1" = power > 0 && power < 1)
  if (!is.null(seed)) stopifnot("seed must be > 0" = seed > 0)

  #ceiling(2 * (sd * multz(1 - power, ngroups - 1, 0.5, seed) / dif)^2)
  2 * (sd * multz(1 - power, ngroups - 1, 0.5, seed) / dif)^2
}


#' Power calculation for the Indifferent-zone approach for normal outcomes
#'
#' Estimate the probability of correctly select the best group among
#' `ngroups` groups  if the difference between the best group and the next best
#'  is at least `dif`(the Indifferent-Zone), and the standard deviation is `sd`
#'
#' @param dif Numeric. Indifferent-zone. Minimum difference that is considered meaningful.
#' @param sd Numeric. Common standard deviation of the response variable.
#' @param ngroups Integer. Number of groups (treatments) being compared.
#' @param npergroup Integer. Number in each group.
#' @param seed Optional. Integer seed to use in the internal call to \code{multp()}.
#'
#' @return Integer. Sample size required per group to achieve the specified power.
#'
#' @note
#' The function uses the quantile function \code{multp()}, which computes critical values
#' for the selection procedure. This implementation assumes equal variances and independent samples.
#' @examples
#' power_best_normal(dif = 0.5, sd = 1, ngroups = 3, npergroup = 11)
#'
#' @export
power_best_normal <- function(dif, sd, ngroups, npergroup, seed = NULL) {
  
  stopifnot("ngroups must be >=2 0" = ngroups >= 2)
  stopifnot("dif must be > 0" = dif > 0)
  stopifnot("sd must be > 0" = sd > 0)
  stopifnot("npergroup must be > 0" = npergroup > 0)
  if (!is.null(seed)) stopifnot("seed must be > 0" = seed > 0)
  
  1-multp((sqrt(npergroup/2)*dif)/sd,ngroups-1,0.5, seed)
}
