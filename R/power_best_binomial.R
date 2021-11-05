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
#' @param prob the probability  in the rest of the groups for each outcome
#' @param dif difference between the most promising and the rest of the groups for each outcome
#' @param ngroups number of groups to compare
#' @param npergroup number of subjects in each group
#' @param nsimul number of simulations
#' @importFrom stats rbinom
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
#'   prob = 0.5
#'   dif = 0.2,
#'   ngroups= 3,
#'   npergroup= 30,
#'   nsimul=10000
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

    if (length(npergroup == 1))
      npergroup = rep(npergroup, ngroups)

    # Probability in the best group
    prob_best <- prob + dif
    stopifnot("Prob + dif should be greater than 0 and lower than 1"=
      all(prob_best < 1) & all(prob_best > 0))
    stopifnot("noutcomes must be greater than 0"=
                noutcomes > 0)

    # Vector of the probabilities for simulations
    probvec <- vector()
    for (i in 1:noutcomes) {
      probvec <- c(probvec,prob_best[i], rep(prob[i],ngroups-1))
    }

    ## To confirm the correct disposition of probvec
    # aprob<-array(probvec,dim=c(ngroups, noutcomes))
    # aprob

    # Vector of the sizes
    sizevec <- vector()
    for (i in 1:noutcomes) {
        sizevec <- c(sizevec, npergroup)
    }
    ## To confirm the correct disposition of values
    #asize<-array(sizevec,dim=c(ngroups,noutcomes))
    #asize

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
                best_group <-
                  apply(simulone,2,function(x){sample(which(x == max(x)),1)})
                ifelse(all(best_group == 1), 1, 0)
             }, 0.0 )

    # Calculate the power
    mean(simrest)
  }


#' Probability with lowest power to detect a difference
#'
#' Estimates the probability of the less promising groups with the
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
    filter(prob+dif < 1)

  # The simulations
  sim_res <-
    sim_matrix %>%
    ddply(
      .(prob,dif,ngroups,npergroup,nsimul),
      function(x){
        data.frame(
          power = power_best_binomial(
            noutcomes = 1,
            prob = x$prob,
            dif = x$dif,
            ngroups = x$ngroups,
            npergroup = x$npergroup,
            nsimul = x$nsimul
          )
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
      "The probability in the least favorable groups with lowest power to",
      "detect the best groups with a difference of ",x$dif, "among", x$ngroups,
      "groups of", x$npergroup, "participants in each gruop is: ",
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
    aes(x = x$prob, y = x$power*100) + geom_point() +
    geom_line(aes(y=x$pred*100),color = "blue") +
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
    scale_x_continuous("Probability on the least promising groups") +
    scale_y_continuous("Power (%)") +
    theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0))

}

# # to debug
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






