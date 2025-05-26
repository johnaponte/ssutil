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
#' outcomes over others. Weights are automatically scaled to sum to the number
#' of outcomes. The group with the lowest total rank is considered the best.
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
#' @importFrom broom tidy
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
  weights <- weights / sum(weights) * noutcomes
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


#' Least Favorable Configuration in a Binomial Rank Experiment
#'
#' Identifies the event probability in the best group that results in the
#' lowest power to correctly ranked it as best in binomial experiment, 
#' using simulation and quadratic model fitting.
#'
#' @param noutcomes Integer. Number of outcomes.
#' @param dif Numeric. Difference in probabilities between best and other groups.
#' @param weights Numeric vector. Weights for each outcome.
#' @param ngroups Integer. Number of groups.
#' @param npergroup Integer. Sample size per group.
#' @param nsim Integer. Number of simulations.
#' @param p1 Optional vector of probabilities for the best group. If missing,
#' defaults to a sequence from 0.01 to 0.99.
#'
#' @return An S3 object of class \code{prob_lowest_power_bin_rank} with:
#' \itemize{
#'   \item \code{minprob}: probability in best group with lowest power
#'   \item \code{minpow}: estimated lowest power
#'   \item \code{simulation}: simulation results with predicted curve
#' }
#'
#' @importFrom purrr map
#' @importFrom dplyr filter mutate ungroup select
#' @importFrom tidyr nest unnest
#' @importFrom stats glm coef predict
#' @export
lf_config_bin_rank <- function(
    noutcomes,
    dif,
    weights,
    ngroups,
    npergroup,
    nsim,
    p1 = seq(0.01, 0.99, length.out = 50)
) {
  sim_matrix <- expand.grid(p1 = p1, dif = dif) |>
    filter(p1 - dif > 0 & p1 - dif < 1)

  sim_res <- sim_matrix |>
    mutate(idsim = row_number()) |>
    nest(data = -idsim) |>
    ungroup() |>
    mutate(power = map(data, ~sim_power_best_bin_rank(
      noutcomes = noutcomes,
      p1 = .$p1,
      dif = .$dif,
      weights = weights,
      ngroups = ngroups,
      npergroup = npergroup,
      nsim = nsim
    )%>%tidy())) |>
    unnest(c(data, power))

  fit <- glm(power ~ p1 + I(p1^2), data = sim_res)
  minprob <- -coef(fit)["p1"] / (2 * coef(fit)["I(p1^2)"])
  minpow <- predict(fit, data.frame(p1 = minprob))
  sim_res <- mutate(sim_res, pred = predict(fit))

  structure(
    list(
      minprob = minprob,
      minpow = minpow,
      dif = dif,
      ngroups = ngroups,
      npergroup = npergroup,
      simulation = sim_res
    ),
    class = c("lf_config_bin_rank", "list")
  )
}


#' @export
format.lf_config_bin_rank <- function(x, digits = 3, nsmall = 2, ...) {
  paste0(
    "\n",
    "Least favorable configuration\n",
    "------------------------------\n",
    "Scenario Parameters\n",
    "  - Groups: ", x$ngroups, "\n",
    "  - Sample size per group: ", x$npergroup, "\n",
    "  - Difference in event probability (vs best): ", x$dif, "\n",
    "  - Number of simulations per scenario: ", x$nsim, "\n\n",
    " Result\n",
    "  Event probability in the best group resulting in the lowest power: ",
    format(x$minprob, digits = digits, nsmall = nsmall), "\n"
  )
}


#' @export
print.lf_config_bin_rank <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}


#' Plot Power Curve from a `prob_lowest_power_bin_rank` Object
#'
#' Generates a ggplot showing simulated and fitted power curves for selecting
#' the best group based on ranks.
#'
#' @param x An object of class \code{prob_lowest_power_bin_rank}.
#'
#' @return A ggplot2 object.
#' @examples
#' \dontrun{
#' config <- lf_config_bin_rank(dif = 0.2, ngroups = 5, npergroup = 25, nsim = 1000)
#' ggplot_lf_config_bin_rank(config)
#' }
#' @import ggplot2
#' @export
ggplot_lf_config_bin_rank <- function(x) {
  stopifnot("Not an object of class prob_lowest_power_bin_rank!" =
              inherits(x, "lf_config_bin_rank"))
  ggplot(x$simulation) +
    aes(x = p1, y = power * 100) +
    geom_point() +
    geom_line(aes(y = pred * 100), color = "blue") +
    ggtitle(
      "Power to Detect the Best Group Based on Ranks",
      subtitle = paste(
        "Difference:", x$dif,
        "; Groups:", x$ngroups,
        "; N per group:", x$npergroup
      )
    ) +
    labs(caption = paste(
      "Estimated lowest power at p1 =", format(x$minprob, digits = 2, nsmall = 2)
    )) +
    scale_x_continuous("Probability in the Most Promising Group") +
    scale_y_continuous("Power (%)") +
    theme(
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0)
    )
}
