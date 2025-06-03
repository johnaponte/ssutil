
# Simulations for non-inferiority multiple comparisons normal distribution
# 20221213 by JJAV

#' Empirical Power for Non-Inferiority (Normal Outcomes)
#'
#' Estimates empirical power to declare non-inferiority between two groups across multiple outcomes using
#' t-tests. Simulates normally distributed data under the null (no difference) and applies
#' non-inferiority rules based on user-defined required and optional tests.
#'
#' A test is considered non-inferior if the lower bound of its confidence interval is greater
#' than the specified non-inferiority limit. Overall non-inferiority is declared if all
#' \code{test_req} and at least \code{test_opt} of the remaining tests are non-inferior.
#'
#' @param nsim Integer. Number of simulations to perform.
#' @param npergroup Integer. Number of observations per group.
#' @param ntest Integer. Number of tests (outcomes) to compare.
#' @param ni_limit Numeric. Limit to declare non-inferiority. Can be a scalar or vector of length \code{ntest}.
#' @param test_req Integer. Number of required tests that must show non-inferiority (first \code{test_req} tests).
#' @param test_opt Integer. Number of optional tests that must also show non-inferiority from the remaining tests.
#' @param sd Numeric. Standard deviation(s) of the outcomes. Scalar or vector of length \code{ntest}.
#' @param corr Numeric. Correlation between the tests. Scalar (common correlation), or vector of length \code{ntest*(ntest-1)/2}. 
#' @param t_level Numeric. Confidence level used for the t-tests (e.g., 0.95 for 95% CI).scalar or vector of length \code{ntest}.
#' @param conf.level Numeric. Confidence level for the empirical power estimate
#' 
#' @return a S3 object of class \link{empirical_power_result}
#'
#' @note
#'  If only one test is used, correlation is ignored.
#' 
#'  Use correlation 0 for independent outcomes
#'
#'  When using a correlation vector, it must match the number of test pairs:
#'   \code{ntest*(ntest-1)/2}, in this order: (1,2), (1,3), ..., (1,ntest), (2,3), ..., (ntest-1,ntest).
#'
#'  The covariance matrix is derived from the correlation matrix and the standard deviations.
#'
#'  For example: with \code{ntest = 3} and \code{corr = c(0.2, 0.3, 0.4)}, the resulting correlation matrix is:
#'
#'   |         | \[,1\] | \[,2\] | \[,3\] |
#'   |---------|--------|--------|--------|
#'   | \[1, \] | 1      | 0.2    | 0.3    |
#'   | \[2, \] | 0.2.   | 1      | 0.4    |
#'   | \[3, \] | 0.3.   | 0.4    | 1      |
#'
#' @importFrom stats t.test
#' @importFrom MASS mvrnorm
#' @importFrom broom tidy
#' @export
#'
#' @examples
#' \dontrun{
#' sim_power_ni_normal(
#'   nsim = 1000,
#'   npergroup = 250,
#'   ntest = 7,
#'   ni_limit = log10(2/3),
#'   test_req = 2,
#'   test_opt = 3,
#'   sd = 0.4,
#'   corr = 0,
#'   t_level = 0.05
#' )
#' }
sim_power_ni_normal <- function(
    nsim,
    npergroup,
    ntest,
    ni_limit,
    test_req,
    test_opt,
    sd,
    corr = 0,
    t_level = 0.95,
    conf.level = 0.95
) {
  stopifnot("ntest needs to be 1 or bigger" = ntest >= 1)
  stopifnot("npergroup needs to be greater than 0" = npergroup > 0)
  stopifnot("sd needs to be greater than 0" = all(sd > 0))
  stopifnot("test_req cannot be negative" = test_req >= 0)
  stopifnot("test_opt cannot be negative" = test_opt >= 0)
  stopifnot("test_req + test_opt must be <= ntest" = test_req + test_opt <= ntest)
  stopifnot("nsim must be > 0" = nsim > 0)
  stopifnot("t_level must be between 0 and 1" = all(t_level > 0 & t_level < 1))
  stopifnot("conf.level must be between 0 and 1" = all(conf.level > 0 & conf.level < 1))
  
  if (length(sd) == 1) sd <- rep(sd, ntest)
  stopifnot("Length of sd is incorrect" = length(sd) == ntest)

  if (length(ni_limit) == 1) ni_limit <- rep(ni_limit, ntest)
  stopifnot("Length of ni_limit is incorrect" = length(ni_limit) == ntest)

  if (length(t_level) == 1) t_level <- rep(t_level, ntest)
  stopifnot("Length of t_level is incorrect" = length(t_level) == ntest)

  stopifnot("No rules for NI defined" = (test_req + test_opt) > 0)

  # Variance matrix setup
  if (ntest == 1) {
    varm <- sd^2
  } else {
    ncomb <- choose(ntest, 2)
    if (length(corr) == 1) corr <- rep(corr, ncomb)
    stopifnot("Incorrect number of correlations" = length(corr) == ncomb)

    corrm <- diag(ntest)
    corrm[lower.tri(corrm)] <- corr
    corrm[upper.tri(corrm)] <- t(corrm)[upper.tri(corrm)]
    varm <- diag(sd) %*% corrm %*% diag(sd)
  }

  vres <- vapply(1:nsim, function(x) {
    mat1 <- mvrnorm(npergroup, mu = rep(0, ntest), Sigma = varm)
    mat2 <- mvrnorm(npergroup, mu = rep(0, ntest), Sigma = varm)

    lowlim <- vapply(1:ntest, function(y) {
      t.test(mat1[, y], mat2[, y], conf.level = t_level[y])$conf.int[1]
    }, numeric(1))

    nis <- as.integer(lowlim > ni_limit)

    rule_req <- if (test_req > 0) sum(nis[1:test_req]) == test_req else TRUE
    rule_opt <- if (test_opt > 0) sum(nis[(test_req + 1):ntest]) >= test_opt else TRUE

    as.integer(rule_req & rule_opt)
  }, integer(1))

  empirical_power_result(
    x = sum(vres), 
    n = length(vres),
    conf.level = conf.level)
}
