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
#' This implementation approximates Table 1 of the paper using exact binomial confidence intervals
#' via \code{binom.test} and the \code{nBinomial1Sample} function from \pkg{gsDesign}.
#'
#' @param ve_lci Numeric. Lower bound of the current vaccine's efficacy (e.g., 0.95 for 95% VE).
#' @param alpha Numeric. Type I error rate (default = 0.025).
#' @param power Numeric. Desired power for the test (default = 0.90).
#' @param use70 Logical. If \code{TRUE}, assumes at least 30% VE for the new vaccine (the 90–70 rule); otherwise, preserves a fixed fraction of the reference VE.
#' @param preserve Numeric. Proportion of the current vaccine's efficacy to preserve under \code{use70 = FALSE} (default = 0.5).
#'
#' @examples
#' ss_ni_ve(ve_lci = 0.95)
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
#' @importFrom gsDesign nBinomial1Sample
#' @importFrom stats binom.test
#' @export
ss_ni_ve <- function(ve_lci, alpha = 0.025, power = 0.90, use70 = FALSE, preserve = 0.5) {
  stopifnot("ve_lci should be a value between 0 and 1" = ve_lci > 0 & ve_lci < 1)
  stopifnot("alpha should be a value between 0 and 1" = alpha > 0 & alpha < 1)
  stopifnot("power should be a value between 0 and 1" = power > 0 & power < 1)
  stopifnot("ve_lci should be atomic" = is.atomic(ve_lci))
  stopifnot("ve_lci should be a single number" = length(ve_lci) == 1)
  stopifnot("Preserve should be between 0 and 1" = preserve > 0 & preserve < 1)

  hr_uci <- 1 - ve_lci

  delta <- if (use70) {
    1 / (hr_uci / sqrt(0.70))
  } else {
    exp(log(hr_uci) * preserve - log(hr_uci))
  }

  vh0 <- 0
  vhi <- 1 - delta

  ph0 <- (1 - vh0) / (1 - vh0 + 1)
  phi <- (1 - vhi) / (1 - vhi + 1)

  nsize <- nBinomial1Sample(ph0, phi, alpha, 1 - power, n = 1:10000, conservative = TRUE)

  max_hr <- NA
  max_i <- NA
  for (i in 1:nsize) {
    ptest <- binom.test(i, nsize, p = 0.5)
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

