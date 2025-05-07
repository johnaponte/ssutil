# Function to simulate trials and estimate the empirical power from to correctly rank as first the best group
# by JJAV 20211028



#' Power to select as best the best group using simulations
#'
#' This function estimates the empirical power to select correctly as best group
#' the best group, when the outcome follows a binomial distribution, assuming that
#' the probability of the next promising group is at least `dif` lower than the best groups.
#'
#' The function allows to evaluate several outcomes at the same time, in which
#' case evaluates if the most promising group is the best for all outcomes.
#'
#' It assume each outcome follows a binomial distribution and they are independent between them.
#' The outcomes may have the same probability and same difference between the most promising
#' group and the others, or they can be defined differently by outcome. In either case,
#' the most promising has to be the best for all outcomes.
#'
#' The number of subjects per group can be the same or can be specified
#' for each group. If specified, the most promising group is always the first group.
#'
#' @param noutcomes number of outcomes to evaluate
#' @param p1 the probability in the most promising group
#' @param dif difference between the most promising and the rest of the
#'  groups for each outcome
#' @param ngroups number of groups to compare
#' @param npergroup number of subjects in each group
#' @param nsimul number of simulations
#' @importFrom stats rbinom
#' @importFrom stats binom.test
#' @importFrom broom tidy
#' @export
#' @returns The empirical power expressed as the proportion of the simulations
#' where the best group was selected as best for all outcomes.
#' @examples
#' \dontrun{
#' # Power to select the best group if the difference between the best and
#' # the the other two groups is 0.2. One outcome and three groups of 30 subjects
#' #
#' sim_power_best_binomial(
#'   noutcomes = 1,
#'   p1 = 0.7,
#'   dif = 0.2,
#'   ngroups= 3,
#'   npergroup= 30,
#'   nsimul=1000
#'
#' }
sim_power_best_binomial <-
  function(noutcomes,
           p1,
           dif,
           ngroups,
           npergroup,
           nsimul) {

    # Checks and data management
    stopifnot("Incorrect length of npergroup!" =
        length(npergroup) == 1 | length(npergroup) == ngroups)

    stopifnot( "Incorrect length of p1!" =
        length(p1) == 1 | length(p1) == noutcomes)

    stopifnot("Incorrect length of dif!" =
                length(dif) == 1 | length(dif) == noutcomes)

    stopifnot("p1 should be greater than 0 and lower than 1!" =
                all(p1 > 0) & all(p1 < 1))

    stopifnot("dif should be greater than 0!" =
                all(dif > 0))

    stopifnot("noutcomes should be of length 1!" =
                length(noutcomes) == 1)

    stopifnot("ngroups should be of length 1!" =
                length(ngroups) == 1)

    stopifnot("noutcomes should be an integer!" =
                abs(trunc(noutcomes) - noutcomes) < 1e-16)

    stopifnot("ngroups should be an integer!" =
                abs((trunc(ngroups) - ngroups)) < 1e-16)


    if (length(p1) == 1)
      p1 = rep(p1, noutcomes)

    if (length(dif) == 1)
      dif = rep(dif, noutcomes)

    stopifnot("p1 - dif should be greater than 0 and lower than 1"=
                all(p1 - dif < 1) & all(p1-dif > 0))

    stopifnot("noutcomes must be greater than 0"=
                noutcomes > 0)

    if (length(npergroup) == 1)
      npergroup = rep(npergroup, ngroups)

    # p1ability matrix
    probm <- matrix(
      c(p1, rep(p1-dif,ngroups-1)),
      byrow =T,
      ncol = noutcomes)


    # Probability vector
    probvec <- as.vector(probm)

    # matrix of sizes
    sizem <- matrix(rep(npergroup, noutcomes), byrow = F, ncol = noutcomes)
    sizevec <- as.vector(sizem)


    # Simulations of trials
    simrest <-
      vapply(c(1:nsimul),
             function(xx) {
               # Simulate one trial, the result is a multidimensional array
               # of the proportion of events, with
               # dimensions ngroups (rows), noutcomes(cols)
               simulone <-
                 array(
                   rbinom( ngroups * noutcomes,
                         sizevec,
                         probvec) / sizevec,
                   dim = c(ngroups, noutcomes)
                 )

                # Check by column (outcomes) if the first group is the best
                # in all groups (rows). If a tie in max randomly select one
                # if a tie, the return is a list
                best_group <-
                  apply(simulone,2,function(x){which(x == max(x))})
                if (is.list(best_group)){
                  # If is a list select randomly one when more than one
                  best_group = vapply(
                    best_group,
                    function(x){
                      ifelse(length(x)==1,x,sample(x,1))},
                    0)
                }
                # return 1 if the first group is the best of all
                ifelse(all(best_group == 1), 1, 0)
             }, 0.0 )

    # Calculate the power and 95% confidence interval
    out <- tidy(binom.test(sum(simrest), length(simrest)))[,c(1,5,6)]
    names(out)[1]<- "power"
    cbind(out,"nsim" = length(simrest))
  }




#' Least favorable configuration for a binomial experiment
#'
#' Given a difference, a number of groups and a number of subjects per groups, use
#' Monte Carlo simulations to find the least favorable configuration to correctly
#' select the best group.
#'
#' The program estimate a quadratic fit to the empirical power over a grid of
#' potential probabilities for the best group, and identify the probability with
#' the lowest power to correctly identify as best the best group.
#'
#' @examples
#' lf_config(d=0.2, k=4, n=20, simul=1000)
#'
#' @param d indifference zone
#' @param k number of groups
#' @param n number of subjects per group
#' @param simul number of simulations for each scenario
#' @param prob grid of probabilities for the best group. By default is a sequence between 0.05 and 0.95 by 0.01
#' @export
#' @return an S3 object of class lf_config
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom stats glm
#' @importFrom stats coef
#' @importFrom stats predict
#' @importFrom purrr map
lf_config<- function(
  d,
  k,
  n,
  simul,
  prob = seq(from=0.05,to=0.95, by = 0.01)
){

  stopifnot("d should be of length 1!" = length(d)==1)
  stopifnot("k should be of length 1!"= length(k)==1)
  stopifnot("n should be of length 1!" = length(n) == 1)

  # The simulation chain
  sim <-
    expand.grid(
      p1 = prob,
      d = d,
      k = k,
      n = n,
      simul = simul
    ) |>
    filter(p1-d < 1 & p1-d > 0) |>
    mutate(idsim = row_number()) |>
    nest(data= -idsim) |>
    mutate(
      power = map(
        data,
        function(x){
          sim_power_best_binomial(
            noutcomes = 1,
            p1 = x$p1,
            dif= x$d,
            ngroups= x$k,
            npergroup= x$n,
            nsimul=x$simul)})) |>
    unnest(cols = c(data,power))

  # Evaluation of the association between the power and the base probability
  # to find which is the minimum value
  fit <- glm(power~p1 + I(p1^2), data = sim)

  # Find the minimum as the root of the first derivative of the model
  minprob = -coef(fit)["p1"]/coef(fit)["I(p1^2)"]/2

  # Update the simulation results with the prediction of the quadratic model
  sim <- sim |>
    mutate(pred = predict(fit)) |>
    mutate(pred = ifelse(pred > 1, NA, pred))

  # Return the results
  structure(
    list(
      minprob = minprob,
      d = d,
      k = k,
      ns = n,
      simulation = sim
    ),
    class = c("lf_config","list")
  )

}

#' @export
format.lf_config<- function(x, digits = 3, nsmall = 2, ...){
  str <-
    paste(
      "The probability in the most favorable groups with lowest power to",
      "correctly select the best group with a difference of ",x$d, "among", x$k,
      "groups of", x$n, "participants in each group is: ",
      format(x$minprob, digits, nsmall),"\n"
    )
  cat(str)
}

#' @export
print.lf_config<- function(x,...){
  format(x, ...)
  invisible(x)
}

#' GGplot least configuration simulation
#'
#' @param x an object of `lf_config` class
#' @return a ggplot2 object
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{
#' lpb <- lf_config(d = 0.2, k = 5, n = 25, simul = 1000)
#' ggplot_lf_config(lpb)
#' }
ggplot_lf_config <- function(x){
  stopifnot("Not an object of class lf_config!"=
            inherits(x,"lf_config"))
  ggplot(x$simulation) +
    aes(x = p1, y = power*100) + geom_point() +
    geom_line(aes(y=pred*100),color = "blue") +
    ggtitle("Empirical power to detect the best group",
            subtitle =
              paste0(
                "difference: ",
                x$d,
                "; Groups: ",
                x$k,
                "; N per group: ",
                x$n,
                "; Simulations per scenario:",
                x$simul)) +
    labs(caption = paste("Estimated lowest power for probability:", format(x$minprob, digits = 2, nsmall = 2))) +
    scale_x_continuous("Probability of the best group") +
    scale_y_continuous("Power (%)") +
    theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0))

}

# # to debug
# library(tidyverse)
# library(broom)
#p1 = seq(from=0.05,to=0.95, by = 0.01)
#d = 0.2
#k = 4
#n = 20
#simul = 1000

# noutcomes = 1
# p1 = c(0.41)
# dif = c(0.24)
# ngroups= 5
# npergroup= c(25)
# nsimul=10000
#
# # to test
# sim_power_best_binomial(noutcomes,p1,dif,ngroups,npergroup,nsimul)
#
# # Lowest power
# require(plyr)
# require(ggplot2)
# xx <- lf_configuration(dif = 0.2, ngroups = 5, npergroup = 25, nsimul = 10000)
# xx
# ggplot_prob_lowest_power(xx)
#
#
