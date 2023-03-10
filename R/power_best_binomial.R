# Function to simulate trials and estimate empirical power from trials
# by JJAV 20211028



#' Power to select the best group
#'
#' This function estimates the empirical power to select the most promising group as
#' the best group, when the outcome follows a binomial distribution, assuming that
#' the probability of the most promising group is at least `dif` higher than the least promising groups.
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
#' @param prob the probability  in the most promising group
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
#' power_best_binomial(
#'   noutcomes = 1,
#'   prob = 0.5,
#'   dif = 0.2,
#'   ngroups= 3,
#'   npergroup= 30,
#'   nsimul=1000
#'
#' }
power_best_binomial <-
  function(noutcomes,
           prob,
           dif,
           ngroups,
           npergroup,
           nsimul) {

    # Checks and data management
    stopifnot("Incorrect length of npergroup!" =
        length(npergroup) == 1 | length(npergroup) == ngroups)

    stopifnot( "Incorrect length of prob!" =
        length(prob) == 1 | length(prob) == noutcomes)

    stopifnot("Incorrect length of dif!" =
                length(dif) == 1 | length(dif) == noutcomes)

    stopifnot("Prob should be greater than 0 and lower than 1!" =
                all(prob > 0) & all(prob < 1))

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


    if (length(prob) == 1)
      prob = rep(prob, noutcomes)

    if (length(dif == 1))
      dif = rep(dif, noutcomes)

    stopifnot("Prob - dif should be greater than 0 and lower than 1"=
                all(prob - dif < 1) & all(prob-dif > 0))

    stopifnot("noutcomes must be greater than 0"=
                noutcomes > 0)

    if (length(npergroup == 1))
      npergroup = rep(npergroup, ngroups)

    # Probability matrix
    probm <- matrix(
      c(prob, rep(prob-dif,ngroups-1)),
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




#' Probability with lowest power to detect a difference
#'
#' Estimates the probability of the best promising groups with the
#' lowest power to detect a difference between groups.
#' A simulation is made for a grid of probabilities and a quadratic
#' function is fitted to the estimated powers by probability. The
#' function returns the minimum probability from the quadratic function.
#' function.
#' It is restricted to 1 outcome.
#'
#' @param dif difference between the best and the least promising groups
#' @param ngroups number of groups
#' @param npergroup number of subjects per group
#' @param nsimul number of simulations
#' @param prob probabilities on the least promising group to evaluate. By default is a sequence between 0.05 and 0.95 by 0.01
#' @export
#' @return an S3 object of class prob_lowest_power
#' @importFrom plyr ddply
#' @importFrom plyr .
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom stats glm
#' @importFrom stats coef
#' @importFrom stats predict
lowest_prop_best_binomial <- function(
  dif,
  ngroups,
  npergroup,
  nsimul,
  prob = seq(from=0.05,to=0.95, by = 0.01)
){

  stopifnot("dif should be of length 1!" = length(dif)==1)
  stopifnot("ngroups should be of length 1!"= length(ngroups)==1)
  stopifnot("npergroup should be of length 1!" = length(npergroup) == 1)

  # The simulation matrix
  sim_matrix <-
    expand.grid(
      prob = prob,
      dif = dif,
      ngroups = ngroups,
      npergroup = npergroup,
      nsimul = nsimul
    ) %>%
    filter(prob-dif < 1 & prob-dif > 0)

  # The simulations
  sim_res <-
    sim_matrix %>%
    ddply(
      .(prob,dif,ngroups,npergroup,nsimul),
      function(x){
          power = power_best_binomial(
            noutcomes = 1,
            prob = x$prob,
            dif = x$dif,
            ngroups = x$ngroups,
            npergroup = x$npergroup,
            nsimul = x$nsimul
        )
      }
    )

  # Evaluation of the association between the power and the base probability
  # to find which is the minimum value
  fit <- glm(power~prob + I(prob^2), data = sim_res)

  # Find the minimum as the root of the first derivative of the model
  minprob = -coef(fit)["prob"]/coef(fit)["I(prob^2)"]/2

  # Update the simulation results with the prediction of the quadratic model
  sim_res <- sim_res %>%
    mutate(pred = predict(fit))

  # Return the results
  structure(
    list(
      minprob = minprob,
      dif = dif,
      ngroups = ngroups,
      npergroups = npergroup,
      simulation = sim_res
    ),
    class = c("prob_lowest_power","list")
  )

}

#' @export
format.prob_lowest_power<- function(x, digits = 3, nsmall = 2, ...){
  str <-
    paste(
      "The probability in the most favorable groups with lowest power to",
      "detect the best group with a difference of ",x$dif, "among", x$ngroups,
      "groups of", x$npergroup, "participants in each group is: ",
      format(x$minprob, digits, nsmall),"\n"
    )
  cat(str)
}

#' @export
print.prob_lowest_power <- function(x,...){
  format(x, ...)
  invisible(x)
}

#' GGplot of a prob_lowest_power object
#'
#' @param x an object of prob_lower_power class
#' @return a ggplot2 object
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{
#' require(dplyr)
#' require(ggplot2)
#' lpb <- lowest_prop_best_binomial(dif = 0.2, ngroups = 5, npergroup = 25, nsimul = 10000)
#' ggplot_prob_lowest_power(lpb)
#' }
ggplot_prob_lowest_power <- function(x){
  stopifnot("Not an object of class prob_lower_power!"=
            inherits(x,"prob_lowest_power"))
  ggplot(x$simulation) +
    aes(x = prob, y = power*100) + geom_point() +
    geom_line(aes(y=pred*100),color = "blue") +
    ggtitle("Power to detect the best group",
            subtitle =
              paste(
                "Difference: ",
                x$dif,
                "; Groups: ",
                x$ngroups,
                "; N per group: ",
                x$npergroup)) +
    labs(caption = paste("Estimated lowest power for probability:", format(x$minprob, digits = 2, nsmall = 2))) +
    scale_x_continuous("Probability on the most promising groups") +
    scale_y_continuous("Power (%)") +
    theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0))

}

# # to debug
# library(tidyverse)
# library(broom)
# noutcomes = 1
# prob = c(0.41)
# dif = c(0.24)
# ngroups= 5
# npergroup= c(25)
# nsimul=10000
#
# # to test
# power_best_binomial(noutcomes,prob,dif,ngroups,npergroup,nsimul)
#
# # Lowest power
# require(plyr)
# require(ggplot2)
# xx <- lowest_prop_best_binomial(dif = 0.2, ngroups = 5, npergroup = 25, nsimul = 10000)
# xx
# ggplot_prob_lowest_power(xx)
#
#
#
#
#
#
