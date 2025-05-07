#' Calculate Upper Equicoordinate Point for a Multivariate t Distribution
#'
#' This function computes the upper equicoordinate point for a multivariate t
#' distribution with unit variances and a common correlation coefficient.
#' The result is the quantile such that the joint cumulative probability equals \eqn{1 - \alpha}, single tail.
#'
#' @param alpha the significance level (e.g., 0.05 for a 95% confidence level).
#' @param n the number of variables in the multivariate normal distribution.
#' @param rho the common correlation coefficient between the variables.
#' @param df degrees of freedom for the t distribution
#' @param tail select both or lower tail fo the distribution
#' @param seed an object specifying if and how the random number generator should be initialized. Used by the function `qmvt`
#' @return The upper equicoordinate point \eqn{z} such that the probability of all
#' variables being less than or equal to \eqn{z} is \eqn{1 - \alpha}.
#' @examples
#' alpha <- 0.1  # Significance level (10%)
#' n <- 3        # Number of variables
#' rho <- 0.5    # Common correlation coefficient
#' df = 5
#' multt(alpha, n, rho, df)
#' @importFrom mvtnorm qmvt
#' @export
multt <- function(alpha, n, rho, df, tail = c("both.tails", "lower.tail"), seed = NULL) {

  # Use match.arg to validate the tail argument and assign the chosen value
  tail <- match.arg(tail)

  # Create the covariance matrix with unit variances and common correlation
  cov_matrix <- matrix(rho, n, n)   # Create a matrix filled with the correlation rho
  diag(cov_matrix) <- 1             # Set diagonal elements to 1 (unit variances)

  # Define the target quantile as (1 - alpha)
  target_prob <- 1 - alpha

  # calculate the quantile
  z_multt <-
    as.numeric(
      qmvt(
    p = target_prob,
    corr = cov_matrix,
#    tail = "lower.tail",
    tail = tail,
    df = df,
    seed = seed)[1]
   )
  #  Return the result
  return(z_multt)
}

