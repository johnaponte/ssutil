% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/empirical_power_result.R
\name{empirical_power_result}
\alias{empirical_power_result}
\title{Create an Empirical Power Result object}
\usage{
empirical_power_result(x, n, conf.level = 0.95)
}
\arguments{
\item{x}{Number of successes}

\item{n}{Number of trials.}

\item{conf.level}{Confidence level for the returned confidence interval power.}
}
\value{
An object of class \code{empirical_power_result}, a list with components:
\itemize{
\item \code{power}: Estimated power.
\item \code{conf.low}: Lower bound of confidence interval.
\item \code{conf.high}: Upper bound of confidence interval.
\item \code{conf.level}: Confidence level for the returned confidence interval.
\item \code{nsim}: Number of simulations.
}
}
\description{
Constructs an S3 object of class \code{empirical_power_result}, storing the estimated power,
its confidence interval, and the number of simulations used to compute it.
}
\details{
It is a wrap to \code{\link[stats]{binom.test}}
}
\examples{
result <- empirical_power_result(
  x = 10,
  n = 100,
  conf.level = 0.95
)
print(result)
}
