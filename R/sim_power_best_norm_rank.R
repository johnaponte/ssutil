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
#' If `weights` is specified, it is internally rescaled to sum to `noutcomes`.
#' The most promising group is always considered to be the first group.
#'
#' @param noutcomes Integer. Number of outcomes to evaluate.
#' @param sd Numeric vector. Standard deviations for each outcome.
#' @param dif Numeric vector. Difference in means between the best and other groups.
#' @param weights Numeric vector. Weights per outcome (rescaled internally).
#' @param ngroups Integer. Number of groups.
#' @param npergroup Integer or vector. Number of subjects per group.
#' @param nsimul Integer. Number of simulations.
#'
#' @return A data frame with empirical power and 95% confidence interval.
#'
#' @importFrom stats rnorm binom.test
#' @importFrom broom tidy
#' @export
#'
#' @examples
#' \dontrun{
#' sim_power_best_norm_rank(
#'   noutcomes = 3,
#'   sd = c(1, 0.8, 1.5),
#'   dif = c(0.2, 0.15, 0.3),
#'   weights = c(0.5, 0.3, 0.2),
#'   ngroups = 3,
#'   npergroup = c(30, 25, 25),
#'   nsimul = 1000
#' )
#' }
sim_power_best_norm_rank <- function(
    noutcomes,
    sd,
    dif,
    weights,
    ngroups,
    npergroup,
    nsimul
) {
  # Validations
  stopifnot(length(npergroup) == 1 | length(npergroup) == ngroups)
  stopifnot(length(sd) == 1 | length(sd) == noutcomes)
  stopifnot(length(dif) == 1 | length(dif) == noutcomes)
  stopifnot(length(weights) == 1 | length(weights) == noutcomes)
  stopifnot('Dif should be positive' = all(dif > 0))

  # Expand scalars
  if (length(sd) == 1) sd <- rep(sd, noutcomes)
  if (length(dif) == 1) dif <- rep(dif, noutcomes)
  if (length(weights) == 1) weights <- rep(weights, noutcomes)

  # Rescale weights
  weightsc <- weights / sum(weights) * noutcomes
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
  simrest <- vapply(1:nsimul, function(xx) {
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

  out <- tidy(binom.test(sum(simrest), length(simrest)))[, c(1, 5, 6)]
  names(out)[1] <- "power"
  cbind(out, nsim = length(simrest))
}
