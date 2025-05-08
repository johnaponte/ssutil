#' Calculate Event Probability in the Experimental Group Given a Hazard Ratio
#'
#' Computes the event probability in the experimental group based on the event
#' probability in the control group and a specified hazard ratio, assuming
#' proportional hazards.
#'
#' This is useful for sample size calculations, for example in PASS (TM), which does
#' not automatically adjust the event rate for the experimental group.
#'
#' @param p0 Numeric scalar. Probability of an event in the control group (between 0 and 1).
#' @param hr Numeric scalar. Hazard ratio (must be > 0).
#'
#' @return Numeric. The probability of an event in the experimental group.
#'
#' @examples
#' prophr(0.05, 0.6)
#'
#' @export
prophr <- function(p0, hr) {
  if (!is.numeric(p0) )
    stop("p0 must be numeric.")
  if (!is.numeric(hr) )
    stop("hr must be numeric value.")
  if (length(p0) != length(hr))
    stop("p0 and hr must be of the same length")
  if (p0 < 0 || p0 > 1)
    stop("p0 must be between 0 and 1.")
  if (hr <= 0)
    stop("hr must be greater than 0.")

  1 - (1 - p0)^hr
}
