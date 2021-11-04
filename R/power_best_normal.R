# Function to simulate trials and estimate empirical power from trials
# by JJAV 20211028



#' Power to select the best group
#'
#' This function estimates the empirical power to select the most promising group as
#' the best group, when the outcome is normally distributed, assuming that
#' the mean of the most promising group is at least `dif` higher than the other groups.
#'
#' The function allows to evaluate several outcomes at the same time, in which
#' case evaluates if the most promising group is the best for all outcomes.
#'
#' It assume each outcome is normally distributed and they are independent between them.
#' The outcomes may have the same mean, sd and same difference between the most promising
#' group and the others, or they can be defined differently by outcome. In either case,
#' the most promising has to be the best for all outcomes.
#'
#' The number of subjects per group can be the same or can be specified
#' for each group. If specified, the most promising group is always the first group.
#'
#' @param noutcomes number of outcomes to evaluate
#' @param mean mean value for the rest of the groups for each outcome
#' @param sd standard deviation for for each outcome
#' @param dif difference between the most promising and the rest of the groups for each outcome
#' @param ngroups number of groups to compare
#' @param npergroup number of subjects in each group
#' @param nsimul number of simulations
#' @importFrom stats rnorm
#' @export
#' @returns The empirical power expressed as the proportion of the simulations
#' where the best group was selected as best for all outcomes.
#' @examples
#' \dontrun{
#' # Power to select the best group if the difference between the best and
#' # the the other two groups is 0.2. One outcome and three groups of 30 subjects
#' #
#' power_best_normal(
#'   noutcomes = 1,
#'   mean = 1,
#'   sd = 1,
#'   dif = 0.2,
#'   ngroups= 3,
#'   npergroup= 30,
#'   nsimul=10000
#' )
#' #
#' # Power to select the best group if the difference between the best and
#' # the the other two groups is 0.2. three outcomes and three groups of 30 subjects
#' #
#' power_best_normal(
#'   noutcomes = 3,
#'   mean = 1,
#'   sd = 1,
#'   dif = 0.2,
#'   ngroups= 3,
#'   npergroup= 30,
#'   nsimul=10000
#' )
#' #
#' # Power to select the best group if the difference between the best and
#' # the the other two groups is 0.2. three outcome and three groups of 30 subjects
#' # Each outcome has its own parameters
#' #
#' power_best_normal(
#'   noutcomes = 3,
#'   mean = c(1,1.5,2),
#'   sd = c(1,0.8,1.5),
#'   dif = c(0.2,0.15,0.3),
#'   ngroups= 3,
#'   npergroup= 30,
#'   nsimul=10000
#' )
#' #
#' # Power to select the best group if the difference between the best and
#' # the the other two groups is 0.2. three outcomes and three groups, the
#' # first group with 30 subjects and the rest with 25 subjects
#' #
#' power_best_normal(
#'   noutcomes = 3,
#'   mean = 1,
#'   sd = 1,
#'   dif = 0.2,
#'   ngroups= 3,
#'   npergroup= c(30,25,25),
#'   nsimul=10000
#' )
#' }
power_best_normal <-
  function(noutcomes,
           mean,
           sd,
           dif,
           ngroups,
           npergroup,
           nsimul) {

    # Checks and data management
    stopifnot(length(npergroup) == 1 | length(npergroup) == ngroups)
    stopifnot(length(mean) == 1 | length(mean) == noutcomes)
    stopifnot(length(sd) == 1 | length(sd) == noutcomes)
    stopifnot(length(dif) == 1 | length(dif) == noutcomes)

    if (length(mean) == 1)
      mean = rep(mean, noutcomes)
    if (length(sd) == 1)
      sd = rep(sd, noutcomes)
    if (length(dif == 1))
      dif = rep(dif, noutcomes)

    # Maximum number of participants per group
    # Extra observations are removed later
    maxnpergroup <- max(npergroup)

    # Vector of the means
    meanvec <- vector()
    for (i in 1:noutcomes) {
      for (j in 1:ngroups) {
        if (j == 1)
          meanvec <-c(meanvec, rep(mean[i] + dif[i], maxnpergroup))
        else
          meanvec <- c(meanvec, rep(mean[i], maxnpergroup))
      }
    }
    # To confirm the correct disposition of values
    #amv<-array(meanvec,dim=c(maxpnpergroup,pngroups,pnoutcomes))

    # Vector of the standard deviations
    sdvec <- vector()
    for (i in 1:noutcomes) {
      for (j in 1:ngroups) {
        sdvec <- c(sdvec, rep(sd[i], maxnpergroup))
      }
    }
    # To confirm the correct disposition of values
    #asd<-array(sdvec,dim=c(maxpnpergroup,pngroups,pnoutcomes))

    # Simulations of trials
    simrest <-
      vapply(c(1:nsimul),
             function(xx) {
               # Simulate one trial, the result is a multidimensional array with
               # dimensions maxnpergroups, ngroups, noutcomes
               simulone <-
                 array(
                   rnorm(maxnpergroup * ngroups * noutcomes,
                         meanvec,
                         sdvec),
                   dim = c(maxnpergroup, ngroups, noutcomes)
                 )

               # Groups maybe of different size
               # Change to NA if more obs than needed for the group
               if (length(npergroup) > 1) {
                 for (i in 1:length(npergroup)) {
                   if (npergroup[i] < maxnpergroup) {
                     simulone[c((npergroup[i]+1):maxnpergroup), i,] <- NA
                   }
                 }
               }

               # Return 1 of group1 is the best in all outcomes
               is_first_the_best <-
                 vapply(c(1:noutcomes),
                        # a loop for each outcome
                        function(outn) {
                          # the mean of each group but allows different length per group
                          meansone <-
                            apply(simulone[, , outn], 2, mean,na.rm = TRUE)
                          # which row is the best, if a tie, sample 1
                          bestone <-
                            sample(which(meansone == max(meansone)), 1)
                          # True if the best groups is the best
                          ifelse(bestone == 1, TRUE, FALSE)
                        },
                        TRUE)
               ifelse(all(is_first_the_best), 1, 0)
             }, 0.0)

    # Calculate the power
    mean(simrest)
  }

# require(plyr)
# require(ggplot2)
# power_best_normal(
#   noutcomes = 1,
#   mean = 0,
#   sd = 1,
#   dif = 0.2,
#   ngroups= 3,
#   npergroup= 30,
#   nsimul=1000
# )
#
# power_best_normal(
#   noutcomes = 3,
#   mean = 1,
#   sd = 1,
#   dif = 0.2,
#   ngroups= 3,
#   npergroup= 30,
#   nsimul=1000
# )
#
# power_best_normal(
#   noutcomes = 3,
#   mean = c(1,1.5,2),
#   sd = c(1,0.8,1.5),
#   dif = c(0.2,0.15,0.3),
#   ngroups= 3,
#   npergroup= 30,
#   nsimul=1000
# )
#
# power_best_normal(
#   noutcomes = 3,
#   mean = 1,
#   sd = 1,
#   dif = 0.2,
#   ngroups= 3,
#   npergroup= c(30,25,25),
#   nsimul=1000
# )
#
# sim_matrix <-
#   expand.grid(
#     noutcomes = c(1,2,3,4),
#     mean = 0,
#     sd = 0.67,
#     dif = seq(0.05,0.7,by=0.05),
#     ngroups = 3,
#     npergroup = c(25,30,35,40,45,50),
#     nsimul = 1000
#   )
#
# sim_res <-
#   sim_matrix %>%
#   ddply(
#     .(dif,npergroup,noutcomes),
#     function(x){
#       data.frame(
#         power = power_best_normal(
#           noutcomes = x$noutcomes,
#           mean = x$mean,
#           sd = x$sd,
#           dif = x$dif,
#           ngroups = x$ngroups,
#           npergroup = x$npergroup,
#           nsimul = x$nsimul
#         )
#       )
#     }
# )
#
# ggplot(sim_res) +
#    aes( x = dif, y = power, color = as.factor(npergroup)) +
#    geom_line() +
#    facet_wrap(~noutcomes)
#
# # Use an extrapolation to find what is the difference that
# # can be find from the set of outcomes and sample sizes
# finddif<- function(poutcomes, pnpergroup, ppower){
#   # NOTE simres is hardcoded and the filter use a harcoded structure
#   df <- sim_res %>%
#     filter(noutcomes == poutcomes & npergroup==pnpergroup)
#   f <- splinefun(df$power, df$dif, ties = min, method = "monoH.FC")
#   f(ppower)
# }
#
# sim_matrix %>%
#   select(npergroup, noutcomes ) %>%
#   unique() %>%
#   ddply(
#     .(npergroup,noutcomes),
#     function(x){
#       data.frame(
#         power90 = finddif(poutcomes = x$noutcomes, pnpergroup = x$npergroup, ppower = 0.9)
#       )
#     })
#
#
# # To debug the function
# noutcomes = 3
# mean = 1
# sd = 1
# dif = 0.2
# ngroups= 3
# npergroup= 25
# nsimul=1000
