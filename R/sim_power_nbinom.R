#' Empirical Power for Negative Binomial Comparison
#'
#' Estimates empirical power to detect a rate ratio below a specified threshold
#' using a negative binomial model. Simulates count data with overdispersion,
#' fits a model with \code{glm.nb}, and evaluates the power to reject the null
#' hypothesis under the alternative.
#'
#' @param n1 Integer. Number of participants in group 1.
#' @param n2 Integer. Number of participants in group 2.
#' @param ir1 Numeric. Incidence rate in group 1.
#' @param tm Numeric. Average exposure time per subject (assumed equal across subjects).
#' @param rr Numeric. True relative risk between groups (group 2 rate = rr × group 1 rate).
#' @param lowrr Numeric. Lower bound of the relative risk under the null hypothesis.
#' @param dispersion Numeric. Dispersion parameter (\eqn{\phi}) for the negative binomial distribution.
#' @param alpha Numeric. Type I error rate (two-sided).
#' @param nsimul Integer. Number of simulation iterations.
#' @examples
#' \dontrun{
#' sim_power_nbinom(
#'   n1 = 150, n2 = 150,
#'   ir1 = 0.55, tm = 1.7,
#'   rr = 0.6, lowrr = 1,
#'   dispersion = 2,
#'   alpha = 0.025,
#'   nsimul = 1000
#' )
#' }
#' @return A named numeric vector with:
#'
#'  * pwr: Empirical power (proportion of simulations where lowrr is rejected).
#'
#'. * lci: Lower 95\% confidence limit for power.
#'
#'. * uci: Upper 95\% confidence limit for power.
#'
#'. * pw2: Proportion of simulations with p-value < alpha/2 (for reference).
#'
#' @note Uses the alternative parameterization of the negative binomial: \code{mu} is the mean,
#' and \code{size = 1/dispersion}. In \code{glm.nb}, dispersion is estimated as \code{theta}.
#'
#' @author Chris Gast
#' @importFrom MASS glm.nb
#' @importFrom stats qnorm rnbinom binom.test
#' @export

sim_power_nbinom <- function(n1, n2, ir1, tm, rr, lowrr, dispersion, alpha, nsimul) {
  stopifnot(n1 > 0, n2 > 0, ir1 > 0, tm > 0, rr > 0, nsimul > 0, dispersion > 0, alpha > 0)

  pwr <- numeric(nsimul)
  pw2 <- numeric(nsimul)
  qval <- qnorm(1 - alpha / 2)

  for (i in 1:nsimul) {
    df_tmp <- data.frame(
      gp = c(rep(0, n1), rep(1, n2)),
      y = c(
        rnbinom(n = n1, size = 1 / dispersion, mu = ir1 * tm),
        rnbinom(n = n2, size = 1 / dispersion, mu = ir1 * rr * tm)
      ),
      tm = tm
    )

    fit <- MASS::glm.nb(y ~ gp + offset(log(tm)), data = df_tmp)
    sm <- summary(fit)$coef[2, ]  # group effect (log rate ratio)

    # Reject if lower limit is above log(lowrr)
    pwr[i] <- ifelse((lowrr - exp(sm[1] + qval * sm[2])) > 0, 1, 0)
    pw2[i] <- ifelse(sm[4] < alpha / 2, 1, 0)
  }

  btest <- binom.test(sum(pwr), nsimul)
  return(c(
    pwr = unname(btest$estimate),
    lci = btest$conf.int[1],
    uci = btest$conf.int[2],
    pw2 = mean(pw2)
  ))
}

#sim_power_nbinom(n1=150, n2=150, ir1 = .55, tm = 1.7, rr =0.6, lowrr = 1, dispersion = 2, alpha = 0.025, nsimul = 1000)

## NOTE: Need to check lowrr effects and direction
