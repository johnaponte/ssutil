#' Calculate the Upper Equicoordinate Point of a Multivariate Normal Distribution
#'
#' Computes the upper equicoordinate quantile for a multivariate standard normal
#' distribution with unit variances and a common correlation coefficient \code{rho}.
#' That is, it returns the value \eqn{z} such that the joint probability
#' \eqn{P(X_1 \le z, \ldots, X_n \le z) = p}.
#'
#' @param p Numeric. Cumulative probability (e.g., 0.95 for the 95th
#' @param k Integer. Number of variables in the multivariate normal distribution.
#' Must be >= 1.
#' @param rho Numeric. Common correlation coefficient between variables (typically
#' between 0 and 1).
#' @param lower.tail Logical. If \code{TRUE} (default), probability is
#' \eqn{P(X \le z)}; if \code{FALSE}, \eqn{P(X > z)}
#' @param seed Optional. An object specifying if and how the random number generator
#' should be initialized. Passed to \code{\link[mvtnorm]{qmvnorm}}.
#'
#' @return Numeric. The equicoordinate quantile \eqn{z}
#'
#' @examples
#' p <- 0.9    # Significance level (10%)
#' k <- 3      # Number of variables
#' rho <- 0.5  # Common correlation coefficient
#' multz(p, k, rho)
#'
#' @importFrom mvtnorm qmvnorm
#' @export
multz <- function(p, k, rho, lower.tail = TRUE, seed = NULL) {
  stopifnot("k must be >= 1" = k >= 1)
  stopifnot("p must be between 0 and 1" = p > 0 & p < 1)
  
  # Create the covariance matrix with unit variances and common correlation
  cov_matrix <- matrix(rho, k, k)
  diag(cov_matrix) <- 1

  # Define the target cumulative probability
  target_prob <- if (lower.tail) p else 1 - p
  tail_arg    <- "lower.tail"  
  

  # Calculate the equicoordinate quantile
  if (k > 1) {
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

  return(as.numeric(z_multz$quantile))
}


#' Multivariate Normal Equicoordinate Cumulative Distribution Function
#'
#' Computes the joint CDF for a multivariate standard normal distribution
#' with unit variances and a common correlation coefficient \code{rho}.
#' It is the exact functional inverse of \code{multz}:
#' \code{multp(multz(q, k, rho), k, rho) == q}.
#'
#' @param q Numeric. Quantile of the distribution.
#' @param k Integer. Number of variables in the multivariate normal distribution.
#' Must be >= 1.
#' @param rho Numeric. Common correlation coefficient between variables (typically
#' between 0 and 1).
#' @param lower.tail Logical. If \code{TRUE} (default), probability is
#' \eqn{P(X \le q)}; if \code{FALSE}, \eqn{P(X > q)}. Mirrors \code{\link{pnorm}}.
#' @param seed Optional. An object specifying if and how the random number generator
#' should be initialized. Passed to \code{\link[mvtnorm]{pmvnorm}}.
#'
#' @return Numeric. The joint cumulative probability.
#'
#' @examples
#' q <- 1.3      
#' k <- 3        
#' rho <- 0.5    
#' multp(q, k, rho)
#'
#' @importFrom mvtnorm pmvnorm
#' @export
multp <- function(q, k, rho, lower.tail = TRUE, seed = NULL) {
  stopifnot("k must be >= 1" = k >= 1)
  
  # Create the covariance matrix with unit variances and common correlation
  cov_matrix <- matrix(rho, k, k)
  diag(cov_matrix) <- 1
  
  # Calculate the probability
  if (k > 1) {
    p_multz <- pmvnorm(
      lower = -Inf,
      upper = rep(q,k),
      corr = cov_matrix,
      mean = 0,
      seed = seed
    )
  } else {
    p_multz <- pmvnorm(
      lower = -Inf,
      upper = rep(q,k),
      sigma = 1,
      mean = 0,
      seed = seed
    )
  }
  
  prob <- as.numeric(p_multz[1])
  if (!lower.tail) prob <- 1 - prob

  return(prob)
}

# multp(multz(0.80,3,0.5),3,0.5) #~0.8
