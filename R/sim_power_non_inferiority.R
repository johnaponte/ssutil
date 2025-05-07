
# Simulations for non-inferiority multiple comparisons normal distribution
# 20221213 by JJAV

#' Empirical power for non-inferiority
#'
#' Estimate the empirical power to detect non-inferiority assuming a true
#' no difference in normally distributed outcomes, using t.test for several tests.
#' Non-inferiority for a single test is defined as having a lower confidence
#' interval of the comparison is higher than the non-inferiority limit.
#' Overall non-inferiority is declared as a rule for specific test or a number
#' of test from the group (ie 3 of the five are non-inferior)
#'
#' @param nsimul number of simulations
#' @param npergroup number of observations per group
#' @param ntest number of test to compare
#' @param ni_limit limit to declare non-inferiority of a test. It can be a single number of a vector with limits for each test
#' @param test_req Number of test required to accomplish non-inferiority. Always the first tests
#' @param test_opt Number of the test that must accomplish non-inferiority from the rest of
#'                  the test non required
#' @param sd standard deviation of the distribution. can be a single number for all test or a vector with sd for each test
#' @param corr correlation between the tests. If not included or 0, not correlation is included.
#'             if a single value, similar correlation between all test. Otherwise a vector of correlation between each pair of test
#' @param conf.level confidence level of the interval. One number for all test or a vector to specify a different conf.level for each test
#' @return a tibble with the power to declare non inferiority and 95% CI around the power
#' @importFrom stats t.test
#' @importFrom MASS mvrnorm
#' @importFrom broom tidy
#' @export
#' @note
#'
#'   If only one test, correlation is not taken into account.
#'
#'
#'   If correlation is specified as a vector, it must represent all combinations
#'   between test in order: i.e. (1,2),(1,3),(1,4),..,(1,ntest),(2,3),(2,4)..(2,ntest)... (1-ntest,1-ntest)
#'
#'   For example with 3 test with correlation between test = c(0.2,0.3,0.4)
#'   would produce the correlation matrix:
#'
#'   |        | \[ ,1\] | \[ ,2\] | \[ ,3\] |
#'   |:------:|:------:|:------:|:------:|
#'   | \[1, \] |  1     | 0.2    | 0.3    |
#'   | \[2, \] | 0.2    |  1     | 0.4    |
#'   | \[3, \] | 0.3    | 0.4    |  1     |
#'
#' @examples
#' # Non-Inferiority for 7 test, from which the first two must accomplish
#' # non-inferiority and 3 of the left 5 must accomplish non inferiority
#' # sd and ni_limit is the same for the 7 tests. sd is in log10 scale
#'
#' sim_power_ni_normal(
#' nsimul = 1000,
#' npergroup = 250,
#' ntest = 7,
#' ni_limit = log10(2/3),
#' test_req = 2,
#' test_opt = 3,
#' sd = 0.4,
#' corr= 0,
#' conf.level = 0.95
#' )
sim_power_ni_normal <- function(
  nsimul,
  npergroup,
  ntest,
  ni_limit,
  test_req,
  test_opt,
  sd,
  corr = 0,
  conf.level = 0.95
){
  stopifnot("ntest needs to be 1 or bigger"= ntest>=1)
  stopifnot("npergroup needs to be greater than 0"= npergroup > 0)
  stopifnot("sd needs to be greater than 0"= sd >0)
  stopifnot("test_req cannot be negative" = test_req >= 0)
  stopifnot("test_opt cannot be negative"= test_opt >= 0)
  stopifnot("test_req and test_opt bigger than ntest"= test_req + test_opt <= ntest)
  stopifnot("nsimul needs to be greater than 0"= nsimul > 0)
  stopifnot("conf.level needs to be positive"= conf.level > 0)
  stopifnot("conf.level needs to be lower than 1"= conf.level < 1)
  if (length(sd) == 1) sd = rep(sd, ntest)
  stopifnot( "Lenght of sd is incorrect"=length(sd) == ntest)
  if (length(ni_limit)== 1) ni_limit = rep(ni_limit, ntest)
  stopifnot("Lenght of ni_limit is incorrect"= length(ni_limit)==ntest)
  if (length(conf.level)==1) conf.level = rep(conf.level, ntest)
  stopifnot("Lenght of conf.level is incorrect"= length(conf.level)==ntest)
  stopifnot("No rules for NI defined" = (test_req + test_opt) > 0)
  # correlation management
  if (missing(corr)) corr <- 0
  # number of combinations between tests
  if(ntest == 1) {
    # Correlation is not taken into account
    varm = sd^2
  }
  else {
    ncomb <-choose(ntest,2)
    if (length(corr) == 1) corr <- rep(corr,ncomb)
    stopifnot("Incorrect number of correlations"= length(corr) == ncomb)
    # correlation matrix
    corrm <- matrix(NA,ntest,ntest)
    diag(corrm)<-1
    corrm[lower.tri(corrm)]<-corr
    corrm[upper.tri(corrm)]<-corr
    # Covert to variance matrix
    # https://stats.stackexchange.com/questions/62850/obtaining-covariance-matrix-from-correlation-matrix
    varm <-diag(sd) %*% corrm %*% diag(sd)
    #print(corrm)
  }
    vres <-
      vapply(
        1:nsimul,
        function(x){
          # simulate each group
          mat1 <-
            mvrnorm(npergroup, rep(0,ntest), varm)
          mat2 <-
            mvrnorm(npergroup, rep(0,ntest), varm)
          # Find the lower limit for each test
          lowlim <-
            vapply(1:ntest,
                   function(y){
                     yt <- t.test(mat1[,y],mat2[,y], conf.level = conf.level[y])$conf.int[1]
                   },0)
          # non-inferiority for each test
          nis <- ifelse(lowlim > ni_limit,1,0)
          # apply the rules
          if (test_req > 0) {
            rule_req = sum(nis[1:test_req]) == test_req
          } else {
            rule_req = TRUE
          }
          if (test_opt > 0) {
            rule_opt = sum(nis[(test_req+1):ntest]) >= test_opt
          } else {
            rule_opt = TRUE
          }
          # final decision
          ifelse(rule_req & rule_opt, 1, 0)
        }, 0)
    out <- tidy(binom.test(sum(vres), length(vres)))[,c(1,5,6)]
    names(out)[1]<- "power"
    out
}


# sim_power_ni_normal(
# nsimul = 1000,
# npergroup = 250,
# ntest = 7,
# ni_limit = log10(2/3),
# test_req = 2,
# test_opt = 3,
# sd = 0.4,
# conf.level = 0.95
# )
