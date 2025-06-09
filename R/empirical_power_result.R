#' Create an Empirical Power Result object
#'
#' Constructs an S3 object of class \code{empirical_power_result}, storing the estimated power,
#' its confidence interval, and the number of simulations used to compute it.
#' 
#' It is a wrap to \code{\link[stats]{binom.test}}
#'
#' @param x Number of successes
#' @param n Number of trials.
#' @param conf.level Confidence level for the returned confidence interval power.
#'
#' @return An object of class \code{empirical_power_result}, a list with components:
#' \itemize{
#'   \item \code{power}: Estimated power.
#'   \item \code{conf.low}: Lower bound of confidence interval.
#'   \item \code{conf.high}: Upper bound of confidence interval.
#'   \item \code{conf.level}: Confidence level for the returned confidence interval.
#'   \item \code{nsim}: Number of simulations.
#' }
#'
#' @examples
#' result <- empirical_power_result(
#'   x = 10,
#'   n = 100,
#'   conf.level = 0.95
#' )
#' print(result)
#' @export
#' @importFrom stats binom.test 
empirical_power_result <- function(
    x,
    n,
    conf.level = 0.95
) {
  out = binom.test(x,n,conf.level=conf.level)
  structure(
    list(
      power = unname(out$estimate),
      conf.low = unname(out$conf.int[1]),
      conf.high = unname(out$conf.int[2]),
      conf.level = conf.level,
      nsim = n
    ),
    class = "empirical_power_result"
  )
}

#' Print method for empirical_power_result
#'
#' Nicely formats the output of an object of class `empirical_power_result`,
#' showing the power estimate, confidence interval, and number of simulations.
#'
#' @param x An object of class "empirical_power_result".
#' @param ... Further arguments passed to or from other methods (ignored).
#'
#' @return Invisibly returns the object passed in.
#' @export
print.empirical_power_result <- function(x, ...) {
  cat("Empirical Power Result\n")
  cat(strrep("-", 23), "\n")
  cat(sprintf("Power:       %.4f\n", x$power))
  cat(sprintf("%02d%% CI:      [%.4f, %.4f]\n", x$conf.level*100, x$conf.low, x$conf.high))
  cat(sprintf("Simulations: %d\n", x$nsim))
  invisible(x)
}


#' Check if an object is a sim_power_result
#'
#' @param x Any R object.
#' @return Logical. TRUE if `x` inherits from `"sim_power_result"`.
#' @export
is.empirical_power_result <- function(x) {
  inherits(x, "empirical_power_result")
}

#' Tidy Method for empirical_power_result
#'
#' Creates a one-row tibble with the power estimate and confidence interval.
#'
#' @param x A \code{empirical_power_result} object.
#' @param ... Ignored.
#'
#' @return A tibble with columns: \code{power}, \code{conf.low}, 
#'         \code{conf.high},  \code{conf.level}. \code{nsim}.
#'
#' @export
#' @method tidy empirical_power_result
#' @importFrom broom tidy
#' @importFrom tibble tibble
tidy.empirical_power_result <- function(x, ...) {
  tibble::tibble(
    power = x$power,
    conf.low = x$conf.low,
    conf.high = x$conf.high,
    conf.level = x$conf.level,
    nsim = x$nsim,
    method = "Empirical power"
  )
}


