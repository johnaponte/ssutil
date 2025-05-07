# Function to simulate trials and estimate empirical power from trials
# by JJAV 20211028



#' Power to rank as best the best group using simulations
#'
#' This function estimates the empirical power to rank the most promising group as
#' the best group, when the outcome follows a binomial distribution, assuming that
#' the probability of the most promising group is at least `dif` higher than the least promising groups.
#'
#' The function allows to evaluate several outcomes at the same time, in which
#' case evaluates if the most promising group is the best with higher rank
#'
#' It assume each outcome follows a binomial distribution and they are independent between them.
#' The outcomes may have the same probability and same difference between the most promising
#' group and the others, or they can be defined differently by outcome. Differently
#' from power_best_binomial, in which the
#' the most promising has to be the best for all outcomes,
#' this simulation consider the sum of the ranks for each test and return
#' the power to the first group to be selected as the best
#'
#' The outcomes can be weighted to give more importance to some outcomes than
#' others. The weights are scaled to sum the total number of outcomes. A weight
#' of 1 will give the same weight to all outcomes.
#'
#' The number of subjects per group can be the same or can be specified
#' for each group. If specified, the most promising group is always the first group.
#'
#'
#' @param noutcomes number of outcomes to evaluate
#' @param p1 the probability  in the best group
#' @param dif difference with the next best group
#' @param weights weights to rank the outcomes. if 1, same weight is given to all outcomes
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
#' sim_power_best_bin_rank(
#'   noutcomes = 1,
#'   p1 = 0.7,
#'   dif = 0.2,
#'   weights = 1,
#'   ngroups = 3,
#'   npergroup= 30,
#'   nsimul=10000
#'
#' }
sim_power_best_bin_rank <-
  function(
    noutcomes,
    p1,
    dif,
    weights,
    ngroups,
    npergroup,
    nsimul) {

    # Checks and data management
    stopifnot("Incorrect length of npergroup!" =
        length(npergroup) == 1 | length(npergroup) == ngroups)

    stopifnot( "Incorrect length of prob!" =
        length(p1) == 1 | length(p1) == noutcomes)

    stopifnot("Incorrect length of dif!" =
                length(dif) == 1 | length(dif) == noutcomes)

    stopifnot("Prob should be greater than 0 and lower than 1!" =
                all(p1 > 0) & all(p1< 1))

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

    stopifnot(length(weights) == 1 | length(weights) == noutcomes)

    if (length(weights) == 1)
        weights = rep(weights, noutcomes)

    # Weights are scaled to be the number of outcomes
    weightsc = weights/sum(weights)*noutcomes

    # Make the matrix of weight to multiply the ranks
    weightsm <- matrix(rep(weightsc,ngroups), ncol = noutcomes, byrow = T)

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

    # Probability matrix
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
                   rbinom(
                     ngroups * noutcomes,
                     sizevec,
                     probvec) / sizevec,
                   dim = c(ngroups, noutcomes)
                 )
                # Ranks each test
                ranks <- apply(simulone,2,rank,ties.method = "random")
                # Weight the ranks
                ranksw = ranks*weightsm
                # Sum the ranks for each group
                sumranks <- apply(ranksw,1,sum)
                # rank the groups
                rankgroup <- rank(sumranks, ties.method = "random")
                # Return 1 if the max rank is in group 1
                ifelse(rankgroup[1] == ngroups, 1, 0)
             }, 0.0 )

    # Calculate the power and 95% confidence interval
    out <- tidy(binom.test(sum(simrest), length(simrest)))[,c(1,5,6)]
    names(out)[1]<- "power"
    cbind(out,"nsim" = length(simrest))
  }


#' Probability with lowest power to detect a difference using ranks
#'
#' Estimates the probability of the most promising groups with the
#' lowest power to select as best the most promising using ranks
#' A simulation is made for a grid of probabilities for the next bests and
#' a quadratic function is fitted to the estimated powers by probability. The
#' function returns the minimum probability from the quadratic function.
#' function.
#'
#' @param noutcomes number of outcomes to evaluate
#' @param dif difference between the best and the least promising groups
#' @param weights weights to rank the outcomes. if 1, same weight is given to all outcomes
#' @param ngroups number of groups
#' @param npergroup number of subjects per group
#' @param nsimul number of simulations
#' @param p1 probabilities on the best groups. By default is a sequence between 0.05 and 0.95 by 0.01
#' @export
#' @return an S3 object of class prob_lowest_power
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom stats glm
#' @importFrom stats coef
#' @importFrom stats predict
lowest_prop_best_bin_rank <- function(
    noutcomes,
    dif  ,
    weights,
    ngroups,
    npergroup,
    nsimul,
    p1

){

  # The simulation matrix
  sim_matrix <-
    expand.grid(
      p1=  seq(from= 0.01, to= 0.99, length.out = 50) ,
      dif = dif) |>
    filter(p1-dif < 1 & p1-dif > 0 )

  # The simulations
  sim_res <- sim_matrix |>
    mutate(idsim = row_number()) |>
    nest(data = -idsim) |>
    ungroup() |>
    mutate(power = map(data, function(x){
        sim_power_best_bin_rank(
        noutcomes = noutcomes,
        p1 = x$p1,
        dif = x$dif,
        weights = weights,
        ngroups = ngroups,
        npergroup = npergroup,
        nsimul = nsimul)}
    ))|>
      unnest(c(data, power))

  # Evaluation of the association between the power and the base p1ability
  # to find which is the minimum value
  fit <- glm(power~p1 + I(p1^2), data = sim_res)

  # Find the minimum as the root of the first derivative of the model
  minprob = -coef(fit)["p1"]/coef(fit)["I(p1^2)"]/2
  minpow = predict(fit, data.frame(p1 = minprob))

  # Update the simulation results with the prediction of the quadratic model
  sim_res <- sim_res |>
    mutate(pred = predict(fit))

  # Return the results
  structure(
    list(
      minprob = minprob,
      minpow = minpow,
      dif = dif,
      ngroups = ngroups,
      npergroups = npergroup,
      simulation = sim_res
    ),
    class = c("prob_lowest_power_bin_rank","list")
  )

}

#' @export
format.prob_lowest_power_bin_rank<- function(x, digits = 3, nsmall = 2, ...){
  str <-
    paste(
      "The probability in the most promising group with lowest power to",
      "rank as best the most promising group if there is a difference of ",
      x$dif, "with the rest of", x$ngroups, " and",
       x$npergroup, "participants in each group is: ",
      format(x$minprob, digits, nsmall),"\n"
    )
  cat(str)
}

#' @export
print.prob_lowest_power_bin_rank <- function(x,...){
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
ggplot_prob_lowest_power_bin_rank <- function(x){
  stopifnot("Not an object of class prob_lower_power!"=
              inherits(x,"prob_lowest_power_bin_rank"))
  ggplot(x$simulation) +
    aes(x = p1, y = power*100) + geom_point() +
    geom_line(aes(y=pred*100),color = "blue") +
    ggtitle("Power to detect the best group based on ranks",
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
# require(plyr)
# require(tidyverse)
# require(broom)
# require(ggplot2)
#
#noutcomes = 3
#p1 = c(0.7)
#dif = c(0.24)
#weights = c(0.4,0.3,0.1,0.1,0.1)
#groups= 3
#pergroup= c(25)
#
# nsimul=1000
#
# # to test
# sim_power_best_bin_rank(noutcomes,p1,dif, weights,ngroups,npergroup,nsimul)
#
#
# # Find the difference proportion at which we have 90% power to
#xx <- lowest_prop_best_bin_rank(noutcomes= noutcomes, dif = 0.01,  weights = 1, ngroups = ngroups, npergroup= npergroup,nsimul = 1000)
#xx
#ggplot_prob_lowest_power_bin_rank(xx)
#
# # Find the lowest difference with 90% power
#
# minpowdf <-
#   tibble(
#     prob = 0.7,
#     dif =  seq(0.05,0.2, length.out = 100)
#   ) |>
#   filter(prob-dif > 0 ) |>
#   ddply(
#     .(dif),
#     function(x){
#        sim_power_best_bin_rank(
#          noutcomes = 5,
#          prob = x$prob,
#          dif = x$dif,
#          weights = 1,
#          ngroups = 3,
#          npergroup = 25,
#          nsimul = 1000
#        )
#     }
#   )
#
# minpowdf
#
# ggplot(minpowdf) + aes(x=dif, y = power, ymin = conf.low, ymax = conf.high) + geom_point()  + geom_smooth(se = F)
#


