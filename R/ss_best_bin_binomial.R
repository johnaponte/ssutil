#
#' Power to correctly classify as best the best group in a binomial test
#'
#' Estimate the exact probability of correctly classify as best the best group when
#' the outcome follows a binomial distribution, assuming that `p1` is the
#' probability of events in the best group, and the probability in the next
#' group is `dif` lower
#'
#' @param p1 probability in the best group
#' @param dif Difference between the best group and the next
#' @param ngroups Number of groups
#' @param npergroup Number of subjects in each group
#' @return the probability of correctly classify as best the best group
#' @importFrom stats pbinom
#' @importFrom stats dbinom
#' @export
#' @examples
#' power_best_bin_binomial( p1 = 0.8, dif = 0.2, ngroups = 4, npergroup = 50)
#' @references Sobel, Milton, and Marilyn J. Huyett. “Selecting the Best One of
#' Several Binomial Populations.” Bell System Technical Journal 36, no. 2
#' (1957): 537–76. https://doi.org/10.1002/j.1538-7305.1957.tb02411.x.

power_best_bin_binomial <- function(p1, dif, ngroups, npergroup) {

  stopifnot("p1 should be between 0 and 1" = p1 >= 0 & p1 <= 1)
  stopifnot("p1 should be greater than dif" = p1 >= dif)
  stopifnot("p1 - dif should be greater than 0" = (p1-dif) > 0)
  stopifnot("ngroups should be greater than 1" = ngroups > 1)
  stopifnot("ngroups should be an integer" =  abs((trunc(ngroups) - ngroups)) < 1e-16)
  stopifnot("npergroup should be greater than 0" = npergroup > 0)
  stopifnot("npergroup should be an integer" = abs((trunc(npergroup) - npergroup)) < 1e-16)

  d = dif
  k = ngroups
  n = npergroup

  ## Auxiliary function to calculate binomial probabilities
  # choose(n, j) * (p1^j) * ((1 - p1)^(n - j))
  b1j <- function(j, n, p1) {dbinom(j,n,p1)}

  #choose(n, j) * ((p1 - d)^j) * ((1 - p1 + d)^(n - j))
  b2j <- function(j, n, p1, d) {dbinom(j,n,p1-d)}

  # Cumulative probability of b2j from 0 to j
  B2j <- function(j, n, p1, d) {pbinom(j,n,p1-d)}

  Pcs_sum <- 0

  for (j in 0:n) {
    b1j_val <- b1j(j, n, p1)
    inner_sum <- 0

    for (i in 0:(k-1)) {
      coeff <- choose(k-1, i) / (1 + i)
      b2j_val <- b2j(j, n, p1, d)^i
      B2j_val <- B2j(j - 1 - i, n, p1, d)^(k - 1 - i)

      inner_sum <- inner_sum + coeff * b2j_val * B2j_val
    }

    Pcs_sum <- Pcs_sum + b1j_val * inner_sum
  }

  return(Pcs_sum)
}

#' Sample size for selection of the best group in a binomial test
#'
#' Find the minimum sample size per group with a `power`
#' to correctly classify the best group as best from `ngroups` groups if the true
#' probability of events in best group is `p1` and the difference
#' with the next group is `difs`
#'
#' @param power target power
#' @param p1 probability of the best group
#' @param dif difference in probability with the next best group
#' @param ngroups number of groups
#' @param max_n maximum n evaluated
#' @export
#'
ss_best_bin_binomial <- function(power, p1, dif, ngroups, max_n = 1000) {

  stopifnot("Power should be between 0 and 1" = power >= 0 & power  <= 1)
  stopifnot("p1 should be between 0 and 1" = p1 >= 0 & p1 <= 1)
  stopifnot("p1 should be greater than dif" = p1 >= dif)
  stopifnot("p1 - dif should be greater than 0" = (p1-dif) > 0)

  # We search for the best n by iterating over possible values of n
  n = 1
  continue = TRUE
  while(continue & n <= max_n){
    ps <- power_best_bin_binomial(p1,dif,ngroups,n)
    if (ps >= power) {
      continue = FALSE
    }  else {
      n = n + 1
    }
  }
  if ( n > max_n) stop(max_n, ": max_n limit reached!\n")
  return(n)
}
#
# # Example usage:
# # Define parameters
# p1 <- 0.80  # Probability of X(1)
# d <- 0.20   # Adjustment d*
# k <- 4     # Number of processes tied with X(1)
# Pcs_target <- 1  # Target probability for power_best_bin_binomial
#
# # Find the best n
# best_n <- n_best_bin_binomial(Pcs_target, p1, d, k)
# print(paste("Best n:", best_n))




# # Function to calculate binomial coefficient
# binom_coef <- function(n, k) {
#   choose(n, k)
# }
#
# # Function to calculate b1j and b2j
# b1j <- function(j, n, p1) {
#   dbinom(j,n,p1)
#   #return(binom_coef(n, j) * (p1^j) * ((1 - p1)^(n - j)))
# }
#
# b2j <- function(j, n, p1, d) {
#   dbinom(j,n,p1-d)
#   #return(binom_coef(n, j) * ((p1 - d)^j) * ((1 - p1 + d)^(n - j)))
# }
#
# # Function to calculate B2_j based on cumulative probabilities
# B2j <- function(j, n, p1, d) {
#   pbinom(j,n,p1-d)
#   # prob_sum <- 0
#   # for (i in 0:j) {
#   #   prob_sum <- prob_sum + b2j(i, n, p1, d)
#   # }
#   # return(prob_sum)
# }
