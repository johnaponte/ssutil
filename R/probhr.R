#' Calculate the probability of event given a hazard ratio
#'
#' This calculate the probability of event in the experimental group
#' based on the probability of event in the control group and the hazard ratio,
#' assuming proportionality of the hazard assumption.
#' @param p0 Probability of an event in the control group
#' @param hr Hazard ratio
#' @return The probability of event in the experimental group
#' @export
#' @examples
#' probhr(0.05,0.6)
probhr <- function(p0, hr){
  1-(1-p0)^hr
}
