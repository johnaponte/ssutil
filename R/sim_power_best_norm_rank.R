# Function to simulate trials and estimate empirical power from trials
# by JJAV 20211028


#' Simulate Power to Select Best Group by Ranks (Normal Outcomes)
#'
#' Estimates the empirical power to identify the most promising group as best,
#' using weighted ranks across outcomes, assuming normally distributed outcomes.
#'
#' Each outcome is independent and normally distributed. The most promising group
#' is assumed to have a mean at least `dif` higher than the others. Ranks are
#' weighted and summed per group across outcomes.
#'
#' If `weights` is specified, it is internally scaled to sum to 1.
#' The most promising group is always considered to be the first group.
#'
#' @param noutcomes Integer. Number of outcomes to evaluate.
#' @param sd Numeric vector. Standard deviations for each outcome.
#' @param dif Numeric vector. Difference in means between the best and other groups.
#' @param weights Numeric vector. Weights per outcome.
#' @param ngroups Integer. Number of groups.
#' @param npergroup Integer or vector. Number of subjects per group.
#' @param nsim Integer. Number of simulations.
#' @param conf.level Numeric. Confidence level for the empirical power estimate
#' 
#' @return An S3 object of class \code{empirical_power_result}, which contains
#'   the estimated empirical power and its confidence interval. The object can
#'   be printed, formatted, or further processed using associated S3 methods.
#'   See also \code{\link{empirical_power_result}}.
#' @seealso \code{\link{empirical_power_result}}
#'
#' @importFrom stats rnorm binom.test
#' @export
#'
#' @examples
#' sim_power_best_norm_rank(
#'   noutcomes = 3,
#'   sd = c(1, 0.8, 1.5),
#'   dif = c(0.2, 0.15, 0.3),
#'   weights = c(0.5, 0.3, 0.2),
#'   ngroups = 3,
#'   npergroup = c(30, 25, 25),
#'   nsim = 1000
#' )
sim_power_best_norm_rank <- function(
    noutcomes,
    sd,
    dif,
    weights,
    ngroups,
    npergroup,
    nsim,
    conf.level = 0.95
) {
  # Validations
  stopifnot("Incorrect length of npergroup!" =
              length(npergroup) == 1 | length(npergroup) == ngroups)

  stopifnot("Incorrect length of dif!" =
              length(dif) == 1 | length(dif) == noutcomes)
  
  stopifnot("dif should be greater than 0!" = all(dif > 0))
  
  stopifnot("noutcomes and ngroups must be scalars!" =
              length(noutcomes) == 1 & length(ngroups) == 1)
  
  stopifnot("noutcomes and ngroups must be integers!" =
              all(abs(trunc(c(noutcomes, ngroups)) - c(noutcomes, ngroups)) < 1e-16))
  
  stopifnot("Incorrect length of sd" = length(sd) == 1 | length(sd) == noutcomes)
  
  stopifnot("Incorrect length of weights" = length(weights) == 1 | length(weights) == noutcomes)
  
  stopifnot("ngroups must be at least 2" = ngroups >= 2)
  
  stopifnot("All npergroup values must be >= 1" = all(npergroup >= 1))
  

  # Expand scalars
  if (length(sd) == 1) sd <- rep(sd, noutcomes)
  if (length(dif) == 1) dif <- rep(dif, noutcomes)
  if (length(weights) == 1) weights <- rep(weights, noutcomes)

  # Rescale weights
  weightsc <- weights / sum(weights)
  weightsm <- matrix(rep(weightsc, ngroups), ncol = noutcomes, byrow = TRUE)

  # Handle group sizes
  if (length(npergroup) == 1) npergroup <- rep(npergroup, ngroups)
  maxn <- max(npergroup)

  # Construct mean and SD vectors for simulation
  meanvec <- unlist(lapply(1:noutcomes, function(i) {
    c(rep(0, maxn), rep(-dif[i], maxn * (ngroups - 1)))
  }))

  sdvec <- unlist(lapply(1:noutcomes, function(i) {
    rep(sd[i], maxn * ngroups)
  }))

  # Run simulations
  simrest <- vapply(1:nsim, function(xx) {
    simulone <- array(
      rnorm(maxn * ngroups * noutcomes, mean = meanvec, sd = sdvec),
      dim = c(maxn, ngroups, noutcomes)
    )

    # Handle unequal group sizes
    if (any(npergroup < maxn)) {
      for (i in seq_along(npergroup)) {
        if (npergroup[i] < maxn) {
          simulone[(npergroup[i] + 1):maxn, i, ] <- NA
        }
      }
    }

    # Rank by outcome
    ranks <-
      sapply(
        1:noutcomes,
        function(outn) {
            means <- colMeans(simulone[, , outn], na.rm = TRUE)
            rank(means, ties.method = "random")
        }
      )

    # Weighted sum of ranks
    wranks <- ranks * weightsm
    sumranks <- rowSums(wranks)
    rankgroup <- rank(sumranks, ties.method = "random")

    as.integer(rankgroup[1] == ngroups)
  }, 0.0)

  empirical_power_result(
    x =sum(simrest), 
    n= length(simrest), 
    conf.level = conf.level)
}
