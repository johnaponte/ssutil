# Sample size for safety
# 20211101 by JJAVE
require(stringr)

#' Rate of events to observe at least one event
#'
#' Estimate which is the true proportion required to observe
#' at least one event given a sample size and a power
#'
#' @param subjects sample size
#' @param power power
#' @return and s3 object of class power_single_rate with the proportion that can be
#' detected with the specified power and sample size
#' @export
#' @importFrom stringr str_pad
#' @importFrom stats binom.test
#' @examples
#' power_single_rate(30,0.9)
#' power_single_rate(c(30,50,100), c(0.9))
power_single_rate<- function(subjects, power){
  # Check parameters
  res <- numeric()
  for (ni in subjects) {
    for (powerj in power) {
      stopifnot(as.integer(ni)==ni)
      stopifnot(0 < powerj & powerj < 1)
      #ss <-uniroot(function(x){powerj-(1-dbinom(0,ni,x))}, interval=c(0,1))$root
      ss<- binom.test(0,ni,p=powerj, alternative = "less")$conf.int[2]
      res = c(res, ni, powerj,ss)
    }
  }
  rem <- matrix(res, ncol=3, byrow = T)
  colnames(rem)<- c("n","power","proportion")
  class(rem)<- c(class(rem),"power_single_rate")
  rem
}



#' @export
format.power_single_rate <- function(x, digits=3, ...){
  stopifnot(inherits(x,"power_single_rate"))
  scale <- -floor(log10(x[,3]))
  val = x[,3]*(10^scale)
  valf <- format(val, digits=digits, ...)
  if (nrow(x) == 1) {
    res_str <-
    paste(
      "An study with ",
    x[1,1],
      " participants would have ",
    x[1,2]*100,
    "% power to detect at least one",
    " event if the true proportion of",
    " events is at least ",
    valf,
    " per ",
    10^scale,
    " participants.",
    sep = "")
  } else {
      res_str <-
        paste("According to the number of participants, ",
          "the table shows the power to detect at\n ",
          "least event given a true proportion of events or higher\n\n",
          sep = " ")
      subjects <- x[,1]
      powert <- paste0(x[,2]*100,"%")
      restext <-  paste(valf, "per",10^scale,"participants")
      width1 = max(8, max(nchar(subjects)))
      width2 = max(5, max(nchar(powert)))
      width3 = max(10, max(nchar(restext)))
      res_str <-
        paste(res_str,
          "|",
          str_pad("Subjects", width1),
          "|",
          str_pad("Power", width2),
          "|",
          str_pad("Proportion", width3),
          "|\n", sep = "")

      res_str <-
        paste(res_str,
              "|",
          paste0(rep("-",width1), collapse=""),
          "|",
          paste0(rep("-",width2), collapse=""),
          "|",
          paste0(rep("-",width3), collapse=""),
          "|\n",sep="")


      for (i in c(1:nrow(x))){
        res_str <-
          paste(res_str,
                "|",
          str_pad(subjects[i],width1),
          "|",
          str_pad(powert[i], width2),
          "|",
          str_pad(restext[i],width3),
          "|\n", sep="")
      }
  }
 res_str
}


#' @export
print.power_single_rate<- function(x,...){
  stopifnot(inherits(x,"power_single_rate"))
  cat(format(x,...))
  invisible(x)
}

#
# # Examples
# xx<- power_single_rate(30,0.9)
# xx
#
# yy<- power_single_rate(c(30,50,100), c(0.8))
# yy

