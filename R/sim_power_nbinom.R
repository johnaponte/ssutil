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
#' @param nsim Integer. Number of simulation iterations.
#' @param conf.level Numeric. Confidence level for the empirical power estimate
#' @examples
#' \dontrun{
#' sim_power_nbinom(
#'   n1 = 150, n2 = 150,
#'   ir1 = 0.55, tm = 1.7,
#'   rr = 0.6, lowrr = 1,
#'   dispersion = 2,
#'   alpha = 0.025,
#'   nsim = 1000
#' )
#' }
#' @return an S3 object of class \link{empirical_power_result}
#'
#' @note Uses the alternative parameterization of the negative binomial: \code{mu} is the mean,
#' and \code{size = 1/dispersion}. In \code{glm.nb}, dispersion is estimated as \code{theta}.
#'
#' @author Chris Gast
#' @importFrom MASS glm.nb
#' @importFrom stats qnorm rnbinom 
#' @export

sim_power_nbinom <- function(n1, n2, ir1, tm, rr, lowrr, dispersion, alpha, nsim, conf.level = 0.95) {
  stopifnot(
    n1 > 0,
    n2 > 0,
    ir1 > 0,
    tm > 0,
    rr > 0,
    nsim > 0,
    dispersion > 0,
    alpha > 0,
    conf.level > 0,
    conf.level < 1
  )
  
  pwr <- numeric(nsim)
  #pw2 <- numeric(nsim)
  qval <- qnorm(1 - alpha / 2)

  for (i in 1:nsim) {
    df_tmp <- data.frame(
      gp = c(rep(0, n1), rep(1, n2)),
      y = c(
        rnbinom(n = n1, size = 1 / dispersion, mu = ir1 * tm),
        rnbinom(n = n2, size = 1 / dispersion, mu = ir1 * rr * tm)
      ),
      tm = tm
    )

    fit <- glm.nb(y ~ gp + offset(log(tm)), data = df_tmp)
    sm <- summary(fit)$coef[2, ]  # group effect (log rate ratio)

    # Reject if lower limit is above log(lowrr)
    pwr[i] <- ifelse((lowrr - exp(sm[1] + qval * sm[2])) > 0, 1, 0)
    #pw2[i] <- ifelse(sm[4] < alpha / 2, 1, 0)
  }

  empirical_power_result(
    x = sum(pwr), 
    n = lenght(simrest), 
    conf.level = conf.level)
}

#sim_power_nbinom(n1=150, n2=150, ir1 = .55, tm = 1.7, rr =0.6, lowrr = 1, dispersion = 2, alpha = 0.025, nsim = 1000)

## NOTE: Need to check lowrr effects and direction
