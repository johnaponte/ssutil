# Function to simulate trials and estimate empirical power from trials
# by JJAV 20211028


#' Simulate Power to Rank the Best Group Using Binomial Outcomes
#'
#' Estimates the empirical power to rank the most promising group as the best,
#' based on binomial outcomes, via simulation.
#'
#' Each outcome is assumed to follow an independent binomial distribution. The
#' best group is defined as having a probability at least `dif` higher than the
#' other groups. The function sums weighted ranks across multiple outcomes to
#' determine the top group.
#'
#' If multiple outcomes are defined, weights can be applied to prioritize some
#' outcomes over others. Weights are automatically scaled to sum 1. The group 
#' with the lowest total rank is considered the best.
#'
#' @param noutcomes Integer. Number of outcomes to evaluate.
#' @param p1 Numeric. Event probability in the best group (scalar or vector of length `noutcomes`).
#' @param dif Numeric. Difference between the best group and the rest (scalar or vector of length `noutcomes`).
#' @param weights Numeric vector. Weights for each outcome. If scalar, applied equally.
#' @param ngroups Integer. Number of groups.
#' @param npergroup Integer or vector. Sample size per group.
#' @param nsim Integer. Number of simulations.
#' @param conf.level Numeric. Confidence level for the returned confidence interval
#'
#' @examples
#' \dontrun{
#'   sim_power_best_bin_rank(
#'   noutcomes = 2,
#'   p1 = 0.80,
#'   dif = 0.15,
#'   weights = 1,
#'   ngroups = 3,
#'   npergroup = 30,
#'   nsim = 1000,
#'   conf.level = 0.95)
#' }
#' @return an S3 object of class \link{empirical_power_result}
#'
#' @importFrom stats rbinom binom.test
#' @export
sim_power_best_bin_rank <- function(
    noutcomes,
    p1,
    dif,
    weights,
    ngroups,
    npergroup,
    nsim,
    conf.level = 0.95
) {

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

  stopifnot("Invalid length of weights!" =
              length(weights) == 1 | length(weights) == noutcomes)

  if (length(weights) == 1) weights <- rep(weights, noutcomes)
  weights <- weights / sum(weights) 
  weightsm <- matrix(rep(weights, ngroups), ncol = noutcomes, byrow = TRUE)

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
    ranksw <- ranks * weightsm
    sumranks <- rowSums(ranksw)
    rankgroup <- rank(sumranks, ties.method = "random")
    ifelse(rankgroup[1] == ngroups, 1, 0)
  }, 0.0)

  
  out<-binom.test(sum(simrest), length(simrest), conf.level = conf.level)

  empirical_power_result(
    power = unname(out$estimate),
    conf.low = unname(out$conf.int[1]),
    conf.high = unname(out$conf.int[2]),
    conf.level = conf.level,
    nsim = nsim
  )
}

