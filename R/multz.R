#' Calculate the Upper Equicoordinate Point of a Multivariate Normal Distribution
#'
#' Computes the upper equicoordinate quantile for a multivariate standard normal
#' distribution with unit variances and a common correlation coefficient \code{rho}.
#' That is, it returns the value \eqn{z} such that the joint probability
#' \eqn{P(X_1 \le z, \ldots, X_n \le z) = 1 - \alpha}.
#'
#' @param alpha Numeric. Significance level (e.g., 0.05 for a 95% confidence level).
#' @param k Integer. Number of variables in the multivariate normal distribution.
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
#' k <- 3        # Number of variables
#' rho <- 0.5    # Common correlation coefficient
#' multz(alpha, k, rho)
#'
#' @importFrom mvtnorm qmvnorm
#' @export
multz <- function(alpha, k, rho, seed = NULL) {
  stopifnot("k must be >= 1" = k >= 1)

  # Create the covariance matrix with unit variances and common correlation
  cov_matrix <- matrix(rho, k, k)
  diag(cov_matrix) <- 1

  # Define the target cumulative probability
  target_prob <- 1 - alpha

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

  return(as.numeric(z_multz[1]))
}


#' Calculate the Multivariate Normal Probability
#'
#' Computes the multivariate normal probabilities with arbitrary correlation matrices
#' It is the inverse of the \code{multz} function
#'
#' @param q Numeric. Quantile of the distribution.
#' @param k Integer. Number of variables in the multivariate normal distribution.
#' Must be >= 1.
#' @param rho Numeric. Common correlation coefficient between variables (typically
#' between 0 and 1).
#' @param seed Optional. An object specifying if and how the random number generator
#' should be initialized. Passed to \code{\link[mvtnorm]{pmvnorm}}.
#'
#' @return Numeric. The multivariate probability
#'
#' @examples
#' q <- 1.3      
#' k <- 3        
#' rho <- 0.5    
#' multp(q, k, rho)
#'
#' @importFrom mvtnorm pmvnorm
#' @export
multp <- function(q, k, rho, seed = NULL) {
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
  
  return(1-as.numeric(p_multz[1]))
}

#multp(multz(0.80,3,0.5),3,0.5) #~0.8
