#' Sample Size and Non-Inferiority Margin for Vaccine Efficacy Trials
#'
#' Computes the non-inferiority margin, number of events, and maximum hazard ratio (HR)
#' to declare non-inferiority in vaccine efficacy (VE) trials, based on the approach
#' described by Fleming et al. (2021).
#'
#' @details
#' The method applies either the 95–95 rule or 90–70 rule, depending on whether a minimum
#' VE of 30% is assumed (\code{use70 = TRUE}) or 50% of the current VE is preserved.
#'
#' This implementation approximates Tables 1 and 2 of the paper: the total number of
#' events uses Freedman's (1982) log-rank sample size formula, generalized to allow the
#' true experimental-to-comparator hazard ratio assumed for power (\code{ve_exp}) to
#' differ from 1; the maximum hazard ratio to declare non-inferiority uses an exact
#' binomial confidence interval via \code{binom.test}.
#'
#' @param ve_lci Numeric. Lower bound of the current vaccine's efficacy (e.g., 0.95 for 95% VE).
#' @param alpha Numeric. Type I error rate (default = 0.025).
#' @param power Numeric. Desired power for the test (default = 0.90).
#' @param use70 Logical. If \code{TRUE}, assumes at least 30% VE for the new vaccine (the 90–70 rule); otherwise, preserves a fixed fraction of the reference VE.
#' @param preserve Numeric. Proportion of the current vaccine's efficacy to preserve under \code{use70 = FALSE} (default = 0.5).
#' @param ve_exp Numeric or \code{NULL} (default). Assumed vaccine efficacy of the
#'   experimental vaccine versus placebo, used to compute power for the non-inferiority
#'   trial. If \code{NULL} (the default), the experimental vaccine is assumed to have
#'   the same true efficacy as the active comparator (hazard ratio of 1 between them).
#'   If specified, \code{ve_ac} must also be provided.
#' @param ve_ac Numeric or \code{NULL} (default). Point estimate of the active
#'   comparator's vaccine efficacy versus placebo (as opposed to \code{ve_lci}, which
#'   is its confidence interval lower bound). Only used, and required, when \code{ve_exp}
#'   is specified.
#'
#' @examples
#' # Table 1: rule out margin delta, assuming EXP has the same true efficacy
#' # as AC (175-event active-comparator trial, 95% VE, HR upper 95% CI = 0.0855)
#' ss_ni_ve(ve_lci = 1 - 0.0855, power = 0.9)
#'
#' # The 90-70 rule (use70 = TRUE) rules out the more lenient margin delta0
#' # instead of delta, for the same active-comparator trial as above
#' ss_ni_ve(ve_lci = 1 - 0.0855, power = 0.9, use70 = TRUE)
#'
#' # Table 2: rule out margin delta0, assuming the experimental vaccine has a
#' # fixed 60% efficacy versus placebo regardless of AC's own efficacy (here,
#' # AC has 60% VE, HR upper 95% CI = 0.4997)
#' ss_ni_ve(ve_lci = 1 - 0.4997, power = 0.9, use70 = TRUE, ve_exp = 0.60, ve_ac = 0.60)
#'
#' @return A named list with:
#'
#'   * Upper limit of the HR used to estimate the sample size: Hazard ratio corresponding to \code{ve_lci}.
#'
#'   * Non-inferior margin in HR scale: Non-inferiority margin expressed as a hazard ratio.
#'
#'   * Alpha: The type I error used.
#'
#'   * Power: The power used.
#'
#'   * Total number of events: Total number of events required in the trial.
#'
#'   * Max HR to declare NI: Maximum observed hazard ratio that satisfies the non-inferiority criterion.
#'
#'   * Max number of events in the experimental group: Maximum number of events in the experimental group still compatible with non-inferiority.
#'
#'   * Non-inferior criteria: Description of the applied non-inferiority rule ("At least 30% VE" or "or preserved effect").
#'
#' @references
#' Fleming, T.R., Powers, J.H., & Huang, Y. (2021).
#' The use of active controls and non-inferiority studies in evaluating COVID-19 vaccines.
#' \emph{Clinical Trials}, 18(3), 335–342. \doi{10.1177/1740774520988244}
#'
#' Freedman, L.S. (1982). Tables of the number of patients required in clinical
#' trials using the logrank test. \emph{Statistics in Medicine}, 1(2), 121-129.
#' \doi{10.1002/sim.4780010204}
#'
#' @importFrom stats binom.test qnorm
#' @export
ss_ni_ve <- function(ve_lci, alpha = 0.025, power = 0.90, use70 = FALSE, preserve = 0.5,
                      ve_exp = NULL, ve_ac = NULL) {
  stopifnot("ve_lci should be a value between 0 and 1" = ve_lci > 0 & ve_lci < 1)
  stopifnot("alpha should be a value between 0 and 1" = alpha > 0 & alpha < 1)
  stopifnot("power should be a value between 0 and 1" = power > 0 & power < 1)
  stopifnot("ve_lci should be atomic" = is.atomic(ve_lci))
  stopifnot("ve_lci should be a single number" = length(ve_lci) == 1)
  stopifnot("Preserve should be between 0 and 1" = preserve > 0 & preserve < 1)
  if (!is.null(ve_exp)) {
    stopifnot("ve_exp should be a value between 0 and 1" = ve_exp > 0 & ve_exp < 1)
    stopifnot("ve_ac must be provided when ve_exp is specified" = !is.null(ve_ac))
    stopifnot("ve_ac should be a value between 0 and 1" = ve_ac > 0 & ve_ac < 1)
  }

  hr_uci <- 1 - ve_lci

  delta <- if (use70) {
    1 / (hr_uci / sqrt(0.70))
  } else {
    exp(log(hr_uci) * preserve - log(hr_uci))
  }

  # theta is the true experimental-to-comparator hazard ratio assumed for the
  # power calculation: 1 (equal efficacy) unless ve_exp/ve_ac say otherwise.
  theta <- if (is.null(ve_exp)) 1 else (1 - ve_exp) / (1 - ve_ac)

  # Freedman's (1982) log-rank sample size formula, generalized to testing
  # H0: HR = delta against a true hazard ratio theta (not necessarily 1) by
  # rescaling both by delta: R = delta / theta plays the role of Freedman's
  # "theta" for a null hazard ratio of 1. When theta = 1, R = delta and this
  # is exactly the original (Table 1) formula.
  z_alpha <- qnorm(1 - alpha)
  z_power <- qnorm(power)
  R <- delta / theta
  nsize <- ceiling((z_alpha + z_power)^2 * (R + 1)^2 / (R - 1)^2)

  max_hr <- NA
  max_i <- NA
  # Correct bug also test 0
  for (i in 0:nsize) {
    # Correct bug not using conf.level
    ptest <- binom.test(i, nsize, p = 0.5, conf.level = 1 - 2 * alpha)
    hr <- ptest$estimate / (1 - ptest$estimate)
    mhr_uci <- ptest$conf.int[2] / (1 - ptest$conf.int[2])
    if (mhr_uci < delta) {
      max_hr <- hr
      max_i <- i
    }
  }

  list(
    "Upper limit of the HR used to estimate the sample size" = hr_uci,
    "Non-inferior margin in HR scale" = delta,
    "Alpha" = alpha,
    "Power" = power,
    "Total number of events" = nsize,
    "Max HR to declare NI" = unname(max_hr),
    "Max number of events in the experimental group" = max_i,
    "Non-inferior criteria" = ifelse(
      use70,
      "At least 30% VE in new vaccine",
      paste0("Preserve at least ", preserve*100,"% of the VE in the new vaccine")
    )
  )
}

