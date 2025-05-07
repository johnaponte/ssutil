#' Empirical power for negative binomial
#'
#' Estimates the empirical power for negative binomial comparisons.
#' @author {Chris G}
#' @param nsimul Number of simulations
#' @param n1 Number of participants in group 1
#' @param n2 Number of participants in group 2
#' @param tm Average exposure time
#' @param ir1 Incidence rate in group 1
#' @param rr  Relative risk between group 2 and group 1 (ir2/ir1)
#' @param lowrr Lower limit of the Relative ratio target of the comparison
#' @param dispersion Dispersion parameter
#' @param alpha = Type 1 error (two tails)
#' @importFrom MASS glm.nb
#' @importFrom stats qnorm
#' @importFrom stats rnbinom
#' @note this uses the alternative nomenclature for negative binomial where
#' the size = 1/phi and prob = size/(size + mu) where mu is the mean parameter
#' which for group 1 is mu1 = ir1\*tm and for group 2 is mu2 = ir1\*rr\*tm.
#' size is estimated as theta in `glm.nb`
#' @export
sim_power_nbinom <- function(n1,n2,ir1,tm,rr,lowrr,dispersion,alpha,nsimul){
  pwr <- vector(length = nsimul)
  pw2 <- vector(length = nsimul)
  qval <- qnorm(1-alpha/2)
  for(i in 1:nsimul){
    df_tmp <-
      data.frame(
        gp = c(rep(0,n1),rep(1,n2)),
        y = c(
          rnbinom(n = n1,
                 size = 1/dispersion,
                 mu = ir1*tm),
          rnbinom(n = n2,
                 size = 1/dispersion,
                 mu = ir1*rr*tm)
        ),
        tm = tm
      )
    fit <- glm.nb(y ~ gp + offset(log(tm)), data = df_tmp)
    sm = summary(fit)$coef[2,]
    pwr[i] <- ifelse((lowrr-exp(sm[1]+ qval*sm[2]))>0,1,0)
    pw2[i] <- ifelse(sm[4] < alpha/2, 1,0)
  }
  btest <- binom.test(sum(pwr), nsimul)
  return(c(pwr = unname(btest$estimate), lci = btest$conf.int[1], uci = btest$conf.int[2]), pw2 =mean(pw2))
}

#sim_power_nbinom(n1=150, n2=150, ir1 = .55, tm = 1.7, rr =0.6, lowrr = 1, dispersion = 2, alpha = 0.025, nsimul = 1000)

## NOTE: Need to check lowrr effects and direction
