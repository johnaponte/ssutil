#' Empirical Power for Negative Binomial Comparison
#'
#' Estimates empirical power to detect a relative risk either above or below a specified boundary,
#' depending on the direction of the alternative hypothesis. Simulates count data with over dispersion,
#' fits a model with \code{glm.nb}, and evaluates the power to reject the null
#' hypothesis using a negative binomial model.
#'
#' @param n1 Integer. Number of participants in group 1.
#' @param n2 Integer. Number of participants in group 2.
#' @param ir1 Numeric. Incidence rate in group 1.
#' @param tm Numeric. Average exposure time per subject (assumed equal across subjects).
#' @param rr Numeric. True relative risk between groups (group 2 rate = rr × group 1 rate).
#' @param boundary Numeric. Relative risk boundary under the null hypothesis. 
#' @param dispersion Numeric. Dispersion parameter (\eqn{\phi}) for the negative binomial distribution.
#' @param alpha Numeric. Type I error rate (two-sided).
#' @param nsim Integer. Number of simulation iterations.
#' @param conf.level Numeric. Confidence level for the empirical power estimate
#' @examples
#' sim_power_nbinom(
#'  n1 = 150, n2 = 150,
#'  ir1 = 0.55, tm = 1.7,
#'  rr = 0.6, boundary = 1,
#'  dispersion = 2,
#'  alpha = 0.05,
#'  nsim = 1000
#' )
#' @return an S3 object of class \link{empirical_power_result}
#'
#' @note Uses the alternative parameterization of the negative binomial: \code{mu} is the mean,
#' and \code{size = 1/dispersion}. In \code{glm.nb}, dispersion is estimated as \code{theta}.
#' The 'boundary' parameter defines the relative risk under the null hypothesis. When rr < 1, 
#' rejection occurs if the upper limit of the confidence interval is below the boundary. 
#' When rr > 1, rejection occurs if the lower limit is above the boundary.
#' 
#' The `alpha` parameter is two-sided as it is used to estimate two-sided confidence intervals
#'
#' @author Chris Gast
#' @author John J. Aponte
#' @importFrom MASS glm.nb
#' @importFrom stats rnbinom confint
#' @export
sim_power_nbinom <- function(n1,
                             n2,
                             ir1,
                             tm,
                             rr,
                             boundary,
                             dispersion,
                             alpha,
                             nsim,
                             conf.level = 0.95)
{
  stopifnot(
    n1 > 0,
    n2 > 0,
    ir1 > 0,
    tm > 0,
    rr > 0,
    boundary > 0,
    nsim > 0,
    dispersion > 0,
    alpha > 0,
    conf.level > 0,
    conf.level < 1
  )
  
  pwr <- numeric(nsim)

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
    ci = exp(
      suppressMessages(
        confint(fit, parm = "gp", level = 1-alpha)
      )
    )

    # Reject boundary according to the rr
    if (rr < 1) {
      pwr[i] <- ifelse(ci[2] < boundary, 1, 0)
    } else {
      pwr[i] <- ifelse(ci[1] > boundary, 1, 0)
    }
    #pwr[i] <- ifelse((lowrr - exp(sm[1] + qval * sm[2])) > 0, 1, 0)
    #pw2[i] <- ifelse(sm[4] < alpha / 2, 1, 0)
  }

  empirical_power_result(
    x = sum(pwr), 
    n = length(pwr), 
    conf.level = conf.level)
}

#sim_power_nbinom(n1=150, n2=150, ir1 = .55, tm = 1.7, rr =0.6, boundary = 1, dispersion = 2, alpha = 0.05, nsim = 1000)


