# Function to simulate trials and estimate the empirical power from to correctly rank as first the best group
# by JJAV 20211028


#' Simulate Power to Select the Best Group Using Binomial Outcomes
#'
#' Estimates the empirical power to correctly identify the best group as having
#' the highest outcome, under a binomial distribution. Assumes that the most
#' promising group has a higher success probability than the others by at least
#' `dif`, and that outcomes are independent.
#'
#' Multiple outcomes can be evaluated simultaneously. The power is estimated as
#' the proportion of simulations where the most promising group is selected best
#' in all outcomes.
#'
#' @param noutcomes Integer. Number of outcomes to evaluate.
#' @param p1 Numeric. Probability in the most promising group (scalar or vector).
#' @param dif Numeric. Difference between the best group and the rest.
#' @param ngroups Integer. Number of groups.
#' @param npergroup Integer or vector. Number of subjects per group.
#' @param nsim Integer. Number of simulations.
#' @param conf.level Numeric. Confidence level for the empirical power estimate
#'
#' @return an S3 object of class \link{empirical_power_result}
#'
#' @importFrom stats rbinom 
#' @export
#' @examples
#' \dontrun{
#' sim_power_best_binomial(
#'   noutcomes = 1,
#'   p1 = 0.7,
#'   dif = 0.2,
#'   ngroups = 3,
#'   npergroup = 30,
#'   nsim = 1000
#' )
#' }
sim_power_best_binomial <- function(noutcomes, p1, dif, ngroups, npergroup, nsim, conf.level = 0.95) {
  stopifnot("Incorrect length of npergroup!" =
              length(npergroup) == 1 | length(npergroup) == ngroups)
  
  stopifnot("Incorrect length of p1!" =
              length(p1) == 1 | length(p1) == noutcomes)
  
  stopifnot("Incorrect length of dif!" =
              length(dif) == 1 | length(dif) == noutcomes)
  
  stopifnot("p1 should be between 0 and 1!" =
              all(p1 > 0) & all(p1 < 1))
  
  stopifnot("dif should be greater than 0!" = all(dif > 0))
  
  stopifnot("noutcomes and ngroups must be scalars!" =
              length(noutcomes) == 1 & length(ngroups) == 1)
  
  stopifnot("noutcomes and ngroups must be integers!" =
              all(abs(trunc(c(noutcomes, ngroups)) - c(noutcomes, ngroups)) < 1e-16))
  
  if (length(p1) == 1) p1 <- rep(p1, noutcomes)
  if (length(dif) == 1) dif <- rep(dif, noutcomes)
  
  stopifnot("p1 - dif must be between 0 and 1!" =
              all(p1 - dif > 0 & p1 - dif < 1))
  
  stopifnot("noutcomes must be > 0" = noutcomes > 0)
  
  stopifnot("Invalid confidence interval"= conf.level> 0 & conf.level < 1)
  
  
  if (length(npergroup) == 1) npergroup <- rep(npergroup, ngroups)

  probm <- matrix(c(p1, rep(p1 - dif, ngroups - 1)), byrow = TRUE, ncol = noutcomes)
  probvec <- as.vector(probm)
  sizem <- matrix(rep(npergroup, noutcomes), byrow = FALSE, ncol = noutcomes)
  sizevec <- as.vector(sizem)

  simrest <- vapply(1:nsim, function(xx) {
    simulone <- array(
      rbinom(ngroups * noutcomes, sizevec, probvec) / sizevec,
      dim = c(ngroups, noutcomes)
    )
    ranks <- apply(simulone, 2, rank, ties.method = "random")
    # Success if the first group have the highest rank
    ifelse(all(ranks[1,]== ngroups), 1, 0)
  }, 0.0)

  empirical_power_result(
    x =sum(simrest), 
    n= length(simrest), 
    conf.level = conf.level)
}

