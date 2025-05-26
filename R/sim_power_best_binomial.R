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
#' @param conf.level Numeric. Level for the confidence interval
#'
#' @return A data frame with empirical power and 95% confidence interval.
#'
#' @importFrom stats rbinom binom.test
#' @importFrom broom tidy
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
  stopifnot(length(noutcomes) == 1, length(ngroups) == 1)
  stopifnot(abs(trunc(noutcomes) - noutcomes) < 1e-16)
  stopifnot(abs(trunc(ngroups) - ngroups) < 1e-16)
  stopifnot(length(p1) == 1 | length(p1) == noutcomes)
  stopifnot(length(dif) == 1 | length(dif) == noutcomes)
  stopifnot(all(p1 > 0 & p1 < 1))
  stopifnot(length(npergroup) == 1 | length(npergroup) == ngroups)
  stopifnot("Invalid confidence interval"= conf.level> 0 & conf.level < 1)
  
  if (length(p1) == 1) p1 <- rep(p1, noutcomes)
  if (length(dif) == 1) dif <- rep(dif, noutcomes)
  stopifnot(all(p1 - dif > 0 & p1 - dif < 1))
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
    best_group <- apply(simulone, 2, function(x) which(x == max(x)))
    if (is.list(best_group)) {
      best_group <- vapply(best_group, function(x) ifelse(length(x) == 1, x, sample(x, 1)), 0)
    }
    ifelse(all(best_group == 1), 1, 0)
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


#' Least Favorable Configuration in a Binomial Experiment
#'
#' Identifies the event probability in the best group that results in the
#' lowest power to correctly detect it as best, using simulation and quadratic
#' model fitting.
#'
#' @param dif Numeric. Difference between best and other groups.
#' @param ngroups Integer. Number of groups.
#' @param npergroup Integer. Sample size per group.
#' @param nsim Integer. Number of simulations.
#' @param prob Numeric vector. Grid of probabilities to evaluate.
#'
#' @return An S3 object of class \code{lf_config_binomial} with simulation results and fitted curve.
#'
#' @importFrom dplyr filter mutate row_number select
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom stats glm coef predict
#' @export
#' @examples
#' lf_config_binomial(dif = 0.2, ngroups = 4, npergroup = 20, nsim = 100)
lf_config_binomial <- function(dif,
                               ngroups,
                               npergroup,
                               nsim,
                               prob = seq(0.05, 0.95, by = 0.01)) {

  stopifnot(length(dif) == 1, length(ngroups) == 1, length(npergroup) == 1)
  
  sim <- expand.grid(
    p1 = prob,
    dif = dif,
    ngroups = ngroups,
    npergroup = npergroup,
    nsim = nsim
  ) |>
    filter(p1 - dif > 0 & p1 - dif < 1) |>
    mutate(idsim = row_number()) |>
    nest(data = -idsim) |>
    mutate(powersim = map(
      data,
      ~ sim_power_best_binomial(
        noutcomes = 1,
        p1 = .$p1,
        dif = .$dif,
        ngroups = .$ngroups,
        npergroup = .$npergroup,
        nsim = .$nsim
      ) |> tidy() |>select(-nsim)
    )) |>
    unnest(c(data, powersim))
  
  fit <- glm(power ~ p1 + I(p1^2), data = sim)
  minprob <- -coef(fit)["p1"] / (2 * coef(fit)["I(p1^2)"])
  
  sim_res <- sim |>
    mutate(pred = predict(fit)) |>
    mutate(pred = ifelse(pred > 1, NA, pred))
  
  structure(
    list(
      minprob = minprob,
      dif = dif,
      ngroups = ngroups,
      npergroup = npergroup,
      nsim = nsim,
      simulation = sim_res
    ),
    class = c("lf_config_binomial", "list")
  )
}


#' @export
format.lf_config_binomial <- function(x, digits = 3, nsmall = 2, ...) {
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
print.lf_config_binomial <- function(x, ...) {
  cat(format(x,...))
  invisible(x)
}


#' Plot Least Favorable Configuration
#'
#' Generates a ggplot showing empirical and predicted power to select the best
#' group over a grid of probabilities.
#'
#' @param x An object of class \code{lf_config_binomial}.
#'
#' @return A ggplot object.
#'
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' config <- lf_config_binomial(dif = 0.2, ngroups = 5, npergroup = 25, nsim = 1000)
#' ggplot_lf_config_binomial (config)
#' }
ggplot_lf_config_binomial <- function(x) {
  stopifnot("Not an object of class lf_config!" = inherits(x, "lf_config_binomial"))
  ggplot(x$simulation) +
    aes(x = p1, y = power * 100) +
    geom_point() +
    geom_line(aes(y = pred * 100), color = "blue") +
    ggtitle(
      "Empirical Power to Detect the Best Group",
      subtitle = paste0(
        "Difference: ", x$dif,
        "; Groups: ", x$kgroups,
        "; N per group: ", x$npergroup,
        "; Simulations: ", unique(x$simulation$nsim)
      )
    ) +
    labs(caption = paste("Estimated lowest power for probability:",
                         format(x$minprob, digits = 2, nsmall = 2))) +
    scale_x_continuous("Probability in the Best Group") +
    scale_y_continuous("Power (%)") +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))
}
