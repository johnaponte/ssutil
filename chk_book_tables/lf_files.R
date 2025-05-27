# Removed as they are not usefull

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
        "; Groups: ", x$ngroups,
        "; N per group: ", x$npergroup,
        "; Simulations: ", unique(x$simulation$nsim)
      )
    ) +
    labs(caption = paste("Estimated probability with lowest power:",
                         format(x$minprob, digits = 2, nsmall = 2))) +
    scale_x_continuous("Probability in the Best Group") +
    scale_y_continuous("Power (%)") +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))
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
