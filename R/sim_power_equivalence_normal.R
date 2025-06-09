# Equivalence simulations
# 20211125 by JJAV

#' Empirical Power for Equivalence (Normal Outcomes)
#'
#' Estimates the empirical power to detect equivalence among multiple groups assuming
#' no true difference in normally distributed outcomes. Pairwise two-sample t-tests are
#' used, and equivalence is declared if all confidence intervals for differences between
#' group means lie entirely within the interval defined by \code{llimit} and \code{ulimit}.
#'
#' This function simulates data under the null hypothesis of no difference between groups
#' and calculates the proportion of simulations in which all pairwise comparisons fall within
#' the specified equivalence limits.
#'
#'
#' @examples
#' #Equivalence testing for three groups with log-scale outcome
#' sim_power_equivalence_normal(
#'   ngroups = 3,
#'   npergroup = 172,
#'   sd = 0.403,
#'   llimit = log10(2/3),
#'   ulimit = log10(3/2),
#'   nsim = 1000,
#'   t_level = 0.95
#' )
#' @param ngroups Integer. Number of groups to compare
#' @param npergroup Integer. Number of observations per group.
#' @param sd Numeric. Standard deviation of the outcome distribution (common across groups).
#' @param llimit Numeric. Lower equivalence limit.
#' @param ulimit Numeric. Upper equivalence limit.
#' @param nsim Integer. Number of simulations to perform.
#' @param t_level Numeric. Confidence level used for the t-tests (e.g., 0.95 for 95% CI).
#' @param conf.level Numeric. Confidence level for the empirical power estimate
#' 
#' @return An S3 object of class \code{empirical_power_result}, which contains
#'   the estimated empirical power and its confidence interval. The object can
#'   be printed, formatted, or further processed using associated S3 methods.
#'   See also \code{\link{empirical_power_result}}.
#' @seealso \code{\link{empirical_power_result}}
#'
#' @importFrom stats t.test
#' @importFrom utils combn
#' @export
sim_power_equivalence_normal <- function(
    ngroups,
    npergroup,
    sd,
    llimit,
    ulimit,
    nsim,
    t_level = 0.95,
    conf.level = 0.95
) {
  stopifnot(ngroups >= 2, npergroup >= 1, nsim >= 1, sd > 0)

  vres <- vapply(1:nsim, function(x) {
    mat <- matrix(rnorm(ngroups * npergroup, 0, sd), ncol = ngroups)

    y <- combn(1:ngroups, 2, FUN = function(z) {
      yt <- t.test(mat[, z[1]], mat[, z[2]], conf.level = t_level)
      yt$conf.int[1] >= llimit && yt$conf.int[2] < ulimit
    })
    
    # The same as TOST ###
    #x <- combn(1:ngroups, 2, FUN = function(z) {
    #  xt1 <- t.test(mat[,z[1]], mat[,z[2]], alternative = "greater", mu = llimit)
    #  xt2 <- t.test(mat[,z[1]], mat[,z[2]], alternative = "less", mu = ulimit)
    #  xt1$p.value < 0.025 && xt2$p.value < 0.025
    #})  
    
  
    all(y)
  }, logical(1))


  empirical_power_result(
    x=sum(vres), 
    n= length(vres), 
    conf.level = conf.level)
}
