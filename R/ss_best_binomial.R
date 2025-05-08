#' Power to Correctly Select the Best Group in a Binomial Test
#'
#' Computes the exact probability of correctly identifying the best group
#' when the outcome follows a binomial distribution. It assumes that \code{p1}
#' is the probability of success in the best group, and that the success
#' probability in all other groups is lower by a fixed difference \code{dif}.
#'
#' The formula is based on the exact method described by Sobel and Huyett (1957).
#'
#' @param p1 Numeric. Probability of success in the best group (must be in \[0, 1\]).
#' @param dif Numeric. Difference in success probability between the best group and the next best (must be > 0).
#' @param ngroups Integer. Number of groups (must be greater than 1).
#' @param npergroup Integer. Number of subjects per group (must be positive).
#'
#' @return A numeric value representing the probability of correctly identifying the best group.
#'
#' @importFrom stats pbinom dbinom
#' @export
#'
#' @examples
#' power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 4, npergroup = 50)
#'
#' @references
#' Sobel, M., & Huyett, M. J. (1957). Selecting the Best One of Several Binomial Populations.
#' *Bell System Technical Journal*, 36(2), 537–576. \doi{10.1002/j.1538-7305.1957.tb02411.x}
power_best_binomial <- function(p1, dif, ngroups, npergroup) {
  stopifnot("p1 must be between 0 and 1" = p1 >= 0 & p1 <= 1)
  stopifnot("p1 must be greater than dif" = p1 >= dif)
  stopifnot("p1 - dif must be > 0" = (p1 - dif) > 0)
  stopifnot("ngroups must be greater than 1" = ngroups > 1)
  stopifnot("ngroups must be an integer" = ngroups %% 1 == 0)
  stopifnot("npergroup must be > 0" = npergroup > 0)
  stopifnot("npergroup must be an integer" = npergroup %% 1 == 0)

  d <- dif
  k <- ngroups
  n <- npergroup

  b1j <- function(j, n, p1) dbinom(j, n, p1)
  b2j <- function(j, n, p1, d) dbinom(j, n, p1 - d)
  B2j <- function(j, n, p1, d) pbinom(j, n, p1 - d)

  Pcs_sum <- 0
  for (j in 0:n) {
    b1j_val <- b1j(j, n, p1)
    inner_sum <- 0
    for (i in 0:(k - 1)) {
      coeff <- choose(k - 1, i) / (1 + i)
      b2j_val <- b2j(j, n, p1, d)^i
      B2j_val <- B2j(j - 1 - i, n, p1, d)^(k - 1 - i)
      inner_sum <- inner_sum + coeff * b2j_val * B2j_val
    }
    Pcs_sum <- Pcs_sum + b1j_val * inner_sum
  }

  return(Pcs_sum)
}

#' Sample Size to Select the Best Group in a Binomial Test
#'
#' Computes the minimum sample size per group required to achieve a target probability
#' of correctly selecting the best group in a binomial test. The best group is assumed
#' to have success probability \code{p1}, and the other groups have \code{p1 - dif}.
#'
#' The function searches for the smallest \code{npergroup} such that the power from
#' \code{\link{power_best_binomial}} is at least the target \code{power}.
#'
#' @param power Numeric. Desired probability of correctly selecting the best group (in \[0, 1\]).
#' @param p1 Numeric. Probability of success in the best group (in \[0, 1\]).
#' @param dif Numeric. Difference in success probability with the next best group (> 0).
#' @param ngroups Integer. Number of groups (must be > 1).
#' @param max_n Integer. Maximum sample size to evaluate (default is 1000).
#'
#' @return An integer representing the minimum sample size per group required to reach the specified power.
#'
#' @export
#'
#' @examples
#' ss_best_binomial(power = 0.9, p1 = 0.8, dif = 0.2, ngroups = 4)
ss_best_binomial <- function(power, p1, dif, ngroups, max_n = 1000) {
  stopifnot("power must be between 0 and 1" = power >= 0 & power <= 1)
  stopifnot("p1 must be between 0 and 1" = p1 >= 0 & p1 <= 1)
  stopifnot("p1 must be greater than dif" = p1 >= dif)
  stopifnot("p1 - dif must be > 0" = (p1 - dif) > 0)

  n <- 1
  while (n <= max_n) {
    ps <- power_best_binomial(p1, dif, ngroups, n)
    if (ps >= power) return(n)
    n <- n + 1
  }

  stop(max_n, ": max_n limit reached without achieving desired power.")
}
