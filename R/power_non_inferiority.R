# Simulations for non-inferiority
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
#' @param ntest number of test to compare
#' @param npergroup number of observations per group
#' @param sd standard deviation of the distribution. can be a single number for all test or a vector with sd for each test
#' @param ni_limit limit to declare non-inferiority of a test. It can be a single number of a vector with limits for each test
#' @param test_req Number of test required to accomplish non-inferiority. Always the first tests
#' @param test_opt Number of the test that must accomplish non-inferiority from the rest of
#'                  the test non required
#' @param nsimul number of simulations
#' @param conf.level confidence level of the interval. One number for all test or a vector to specify a different conf.level for each test
#' @return a tibble with the power to declare non inferiority and 95% CI
#' @importFrom stats t.test
#' @export
#' @examples
#' # Non-Inferiority for 7 test, from which the first two must accomplish
#' # non-inferiority and 3 of the left 5 must accomplish non inferiority
#' # sd and ni_limit is the same for the 7 tests. sd is in log10 scale
#'
#' power_ni_normal(
#' ntest = 7,
#' npergroup = 250,
#' sd = 0.4,
#' ni_limit = log10(2/3),
#' test_req = 2,
#' test_opt = 3,
#' nsimul = 1000,
#' conf.level = 0.95
#' )
power_ni_normal <- function(
  ntest,
  npergroup,
  sd,
  ni_limit,
  test_req,
  test_opt,
  nsimul,
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

    vres <-
      vapply(
        1:nsimul,
        function(x){
          # simulate each group
          mat1 <-
            matrix(
              rnorm(ntest*npergroup,0,sd),
              ncol=ntest, byrow = T)
          mat2 <-
            matrix(
              rnorm(ntest*npergroup,0,sd),
              ncol=ntest, byrow = T)
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

# power_ni_normal(
#   ntest = 7,
#   npergroup = 270,
#   sd = 0.50,
#   ni_limit = log10(2/3),
#   test_req = 2,
#   test_opt = 3,
#   nsimul = 1000,
#   conf.level = 0.95
# )

