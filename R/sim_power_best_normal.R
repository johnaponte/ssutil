# Function to simulate trials and estimate empirical power from trials
# by JJAV 20211028


#' Simulate Power to Select Best Group (Normal Outcomes)
#'
#' Estimates the empirical power to identify the most promising group as the best,
#' when outcomes are normally distributed and independent.
#'
#' @details
#' The best group (group 1)
#' is assumed to have mean 0, and the rest of the groups have mean \code{-dif}.
#'
#' Multiple outcomes can be evaluated simultaneously. The power is estimated as the
#' proportion of simulations where the most promising group is the best in all outcomes.
#'
#' The number of subjects per group can be the same or specified per group. In either case,
#' the first group is assumed to be the most promising.
#' @param noutcomes Integer. Number of outcomes to evaluate.
#' @param sd Numeric vector. Standard deviations for each outcome. Can be a single value.
#' @param dif Numeric vector. Difference in means between the best and the other groups.
#' @param ngroups Number of groups to compare.
#' @param npergroup  Number of subjects per group. Can be scalar or vector of length \code{ngroups}.
#' @param nsimul Integer. Number of simulations to perform.
#' @examples
#' \dontrun{
#'   sim_power_best_normal(
#'    noutcomes = 2,
#'    sd = c(1, 1.2),
#'    dif = c(0.2, 0.25),
#'    ngroups = 3,
#'    npergroup = c(30, 25, 25),
#'    nsimul = 1000
#'   )
#' }
#' @return A data frame with

#'|Column    |Description                           |
#'|----------|--------------------------------------|
#'|power	   |Empirical power estimate              |
#'|conf.low  |Lower bound of 95% confidence interval|
#'|conf.high |Upper bound of 95% confidence interval|
#'|nsim      |Number of simulations performed.      |
#'
#' @importFrom stats rnorm binom.test
#' @importFrom broom tidy
#' @export
sim_power_best_normal <- function(
    noutcomes,
    sd,
    dif,
    ngroups,
    npergroup,
    nsimul
) {
  # Validations
  stopifnot("ngroups must be at least 2" = ngroups >= 2)
  stopifnot("All npergroup values must be >= 1" = all(npergroup >= 1))
  stopifnot('Dif should be positive' = all(dif > 0))
  stopifnot(length(npergroup) == 1 | length(npergroup) == ngroups)
  stopifnot(length(sd) == 1 | length(sd) == noutcomes)
  stopifnot(length(dif) == 1 | length(dif) == noutcomes)

  if (length(sd) == 1) sd <- rep(sd, noutcomes)
  if (length(dif) == 1) dif <- rep(dif, noutcomes)
  if (length(npergroup) == 1) npergroup <- rep(npergroup, ngroups)

  maxn <- max(npergroup)

  meanvec <- unlist(lapply(1:noutcomes, function(i) {
    c(rep(0, maxn), rep(-dif[i], maxn * (ngroups - 1)))
  }))

  sdvec <- unlist(lapply(1:noutcomes, function(i) {
    rep(sd[i], maxn * ngroups)
  }))

  simrest <- vapply(1:nsimul, function(xx) {
    simulone <- array(
      rnorm(maxn * ngroups * noutcomes, mean = meanvec, sd = sdvec),
      dim = c(maxn, ngroups, noutcomes)
    )

    if (any(npergroup < maxn)) {
      for (i in seq_along(npergroup)) {
        if (npergroup[i] < maxn) {
          simulone[(npergroup[i] + 1):maxn, i, ] <- NA
        }
      }
    }

    is_first_the_best <- vapply(1:noutcomes, function(outn) {
      means <- colMeans(simulone[, , outn], na.rm = TRUE)
      best_idx <- which(means == max(means))
      if (length(best_idx) > 1) best_idx <- sample(best_idx, 1)
      best_idx == 1
    }, logical(1))

    as.integer(all(is_first_the_best))
  }, 0.0)

  out <- tidy(binom.test(sum(simrest), length(simrest)))[, c(1, 5, 6)]
  names(out)[1] <- "power"
  cbind(out, nsim = length(simrest))
}


