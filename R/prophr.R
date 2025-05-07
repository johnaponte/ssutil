#' Calculate the proportion of events given a hazard ratio
#'
#' This calculate the proportion of events in the experimental group
#' based on the proportion of event in the control group and the hazard ratio,
#' assuming proportionality of the hazard assumption.
#'
#' This is useful when evaluating sample size in PASS(TM) as it does not make the
#' correction of the other group automatically.
#'
#' @param p0 Probability of an event in the control group
#' @param hr Hazard ratio
#' @return The probability of event in the experimental group
#' @export
#' @examples
#' prophr(0.05,0.6)
prophr <- function(p0, hr){
  1-(1-p0)^hr
}
