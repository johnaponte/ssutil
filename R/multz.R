#' Calculate Upper Equicoordinate Point for a Multivariate Normal Distribution
#'
#' This function computes the upper equicoordinate point for a multivariate standard
#' normal distribution with unit variances and a common correlation coefficient.
#' The result is the quantile such that the joint cumulative probability equals \eqn{1 - \alpha}.
#'
#' @param alpha the significance level (e.g., 0.05 for a 95% confidence level).
#' @param n the number of variables in the multivariate normal distribution.
#' @param rho the common correlation coefficient between the variables.
#' @param seed an object specifying if and how the random number generator
#' should be initialized. Used by the function `qmvnorm`
#' @return The upper equicoordinate point \eqn{z} such that the probability of all
#' variables being less than or equal to \eqn{z} is \eqn{1 - \alpha}.
#' @examples
#' alpha <- 0.1  # Significance level (10%)
#' n <- 3        # Number of variables
#' rho <- 0.5    # Common correlation coefficient
#' multz(alpha, n, rho)
#' @importFrom mvtnorm qmvnorm
#' @export
multz <- function(alpha, n, rho, seed = NULL) {

  # Step 1: Create the covariance matrix with unit variances and common correlation
  cov_matrix <- matrix(rho, n, n)   # Create a matrix filled with the correlation rho
  diag(cov_matrix) <- 1             # Set diagonal elements to 1 (unit variances)

  # Step 2: Define the target quantile as (1 - alpha)
  target_prob <- 1 - alpha

  # Step 3: calculate the quantile
  z_multz <-
    as.numeric(
      qmvnorm(
    p = target_prob,
    corr = cov_matrix,
    tail = "lower.tail",
    seed = seed)[1]
   )
  # Step 5: Return the result
  return(z_multz)
}

