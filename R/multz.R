#' Calculate the Upper Equicoordinate Point of a Multivariate Normal Distribution
#'
#' Computes the upper equicoordinate quantile for a multivariate standard normal
#' distribution with unit variances and a common correlation coefficient \code{rho}.
#' That is, it returns the value \eqn{z} such that the joint probability
#' \eqn{P(X_1 \le z, \ldots, X_n \le z) = 1 - \alpha}.
#'
#' @param alpha Numeric. Significance level (e.g., 0.05 for a 95% confidence level).
#' @param n Integer. Number of variables in the multivariate normal distribution.
#' Must be >= 1.
#' @param rho Numeric. Common correlation coefficient between variables (typically
#' between 0 and 1).
#' @param seed Optional. An object specifying if and how the random number generator
#' should be initialized. Passed to \code{\link[mvtnorm]{qmvnorm}}.
#'
#' @return Numeric. The upper equicoordinate point \eqn{z} such that the joint
#' probability of all variables being less than or equal to \eqn{z} is
#' \eqn{1 - \alpha}.
#'
#' @examples
#' alpha <- 0.1  # Significance level (10%)
#' n <- 3        # Number of variables
#' rho <- 0.5    # Common correlation coefficient
#' multz(alpha, n, rho)
#'
#' @importFrom mvtnorm qmvnorm
#' @export
multz <- function(alpha, n, rho, seed = NULL) {
  stopifnot("n must be >= 1" = n >= 1)

  # Create the covariance matrix with unit variances and common correlation
  cov_matrix <- matrix(rho, n, n)
  diag(cov_matrix) <- 1

  # Define the target cumulative probability
  target_prob <- 1 - alpha

  # Calculate the equicoordinate quantile
  if (n > 1) {
    z_multz <- qmvnorm(
      p = target_prob,
      corr = cov_matrix,
      tail = "lower.tail",
      seed = seed
    )
  } else {
    z_multz <- qmvnorm(
      p = target_prob,
      sigma = 1,
      tail = "lower.tail",
      seed = seed
    )
  }

  return(as.numeric(z_multz[1]))
}
