# Equivalence simulations
# 20211125 by JJAV

#' Empirical power for equivalence
#'
#' Estimate the empirical power to detect equivalence assuming a true
#' no difference in normally distributed outcomes, using t.test on all pair
#' comparisons between the different groups. Equivalence is declared if the
#' confidence interval of the difference are within the lower limit (\code{llimit})
#' and the upper limit (\code{ulimit}) for all comparisons
#'
#' @param ngroups number of groups to compare
#' @param npergroup number of observations per group
#' @param sd standard deviation of the distribution
#' @param llimit lower limit for equivalence
#' @param ulimit upper limit for equivalence
#' @param nsimul number of simulations
#' @param conf.level confidence level of the interval
#' @return a tibble with the power to declare equivalence and 95% CI
#' @importFrom stats t.test
#' @importFrom utils combn
#' @importFrom broom tidy
#' @export
#' @examples
#' # Lot to lot equivalence for three lots, assuming a standard deviation
#' # in log10 scale of 0.402 and limits for equivalence log10(2/3) and log10(3/2)
#'
#' power_equivalence_normal(
#' ngroups = 3,
#' npergroup = 172,
#' sd = 0.403,
#' llimit = log10(2/3),
#' ulimit = log10(3/2),
#' nsimul = 1000,
#' conf.level = 0.95
#' )
power_equivalence_normal <- function(
  ngroups,
  npergroup,
  sd,
  llimit,
  ulimit,
  nsimul,
  conf.level = 0.95
){
    vres <-
      vapply(
        1:nsimul,
        function(x){
          mat <-
            matrix(
              rnorm(ngroups*npergroup,0,sd),
              ncol=ngroups)
          y <-
            combn(
              c(1:ngroups),
              2,
              FUN = function(z){
                yt <- t.test(mat[,z[1]],mat[,z[2]], conf.level=conf.level)
                ifelse(yt$conf.int[1]>llimit & yt$conf[2]< ulimit,TRUE,FALSE)
               }
              )
          all(y)
        }, 0)
    out <- tidy(binom.test(sum(vres), length(vres)))[,c(1,5,6)]
    names(out)[1]<- "power"
    cbind(out,"nsim" = length(vres))
    out
}

#
# ngroups = 3
# npergroup = 177
# sd = 0.403
# llimit = log10(2/3)
# ulimit = log10(3/2)
# nsimul = 10000
# conf.level = 0.95
#
# power_equivalence_normal(
#   ngroups = 3,
#   npergroup = 172,
#   sd = 0.403,
#   llimit = log10(2/3),
#   ulimit = log10(3/2),
#   nsimul = 1000,
#   conf.level = 0.95
# )
#
