#' Non inferiority for vaccine efficacy
#'
#' This evaluate the non-inferiority limit, the number of events and
#' the maximum HR to declare inferiority following Fleming et al
#' COVID-19 vaccine trials:
#' The use of active controls and non-inferiority studies. Clinical Trials
#' 18(3), 335-342. https://doi.org/10.1177/1740774520988244
#' following the 95-95 rule
#'
#' Please not that is intended to produce Table 1 of the paper but the results
#' are not exactly similar, I think because rounding and the way the
#' sample 1 binomial is calculated, how the confidence intervals for the
#' proportion are estimated but it is closed enough. Here we use sample1binomial
#' from gsDesign and exact confidence intervals
#' @param ve_lci Lower limit of the vaccine efficacy
#' @param alpha Error type 1
#' @param power Power

#' @importFrom gsDesign nBinomial1Sample
#' @export
#' @return a list with the non-inferiority margin, number of events and maximum
#' hazard ratio and events in the experimental to declare Non inferiority
ni_fleming <- function(ve_lci, alpha = 0.025, power = 0.90){
  stopifnot("ve_lci Should be a value between 0 and 1"= ve_lci > 0 & ve_lci < 1)
  stopifnot("alpha should be a value between 0 and 1" = alpha > 0 & alpha < 1)
  stopifnot("power should be a value between 0 and 1" = power > 0 & power < 1)
  stopifnot("ve_lci should be atomic" = is.atomic(ve_lci))
  stopifnot("ve_lci should be a single number" = length(ve_lci) == 1)

  hr_uci <- 1-ve_lci

  # Non inferior margin
  delta = 1/(hr_uci/sqrt(hr_uci))

  # In VE terms for difference between groups
  vh0 = 0
  vhi = 1-delta

  # In proportion terms
  ph0 = (1-vh0)/(1-vh0 +1)
  phi = (1-vhi)/(1-vhi +1)

  # Number of events using a single proportion problem
  nsize = nBinomial1Sample(ph0,phi,alpha,1-power,n=1:10000, conservative = T)

  # Estimate the maximum ratio that acomplish NI
  max_hr <- NA
  max_i <- NA
  for (i in 1:nsize){
    ptest <- binom.test(i,nsize,p = 0.5)
    hr = ptest$estimate/(1-ptest$estimate)
    hr_uci = ptest$conf.int[2]/(1-ptest$conf.int[2])
    if (hr_uci < delta) {
      max_hr = hr
      max_i = i
    }
  }
  list(
    "Upper limit of the HR used to estimate the sample size" = hr_uci,
    "Non-inferior margin in HR scale" = delta,
    "Alpha" = alpha,
    "Power" = power,
    "Total number of events" = nsize,
    "Max HR to declare NI" = as.numeric(max_hr),
    "Max number of events in the experimental group" = max_i
  )
}

