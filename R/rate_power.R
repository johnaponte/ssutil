# Sample size for safety
# 20211101 by JJAVE
require(stringr)

#' Rate for a given power
#'
#' Estimate which is the true proportion required to observe
#' at least one event given a sample size and a power
#'
#'
#' @param subjects sample size
#' @param power power
#' @return and s3 object of class rate_power with the proportion that can be
#' detected with the specified power and sample size
#' @export
#' @importFrom stringr str_pad
#' @importFrom stats uniroot
#' @importFrom stats dbinom
#' @examples
#' rate_power(30,0.9)
#' rate_power(c(30,50,100), c(0.9))
rate_power<- function(subjects, power){
  # Check parameters
  res <- numeric()
  for (ni in subjects) {
    for (powerj in power) {
      stopifnot(as.integer(ni)==ni)
      stopifnot(0 < powerj & powerj < 1)
      ss <-uniroot(function(x){powerj-(1-dbinom(0,ni,x))}, interval=c(0,1))$root
      res = c(res, ni, powerj,ss)
    }
  }
  rem <- matrix(res, ncol=3, byrow = T)
  colnames(rem)<- c("n","power","proportion")
  class(rem)<- c(class(rem),"rate_power")
  rem
}

#' @export
print.rate_power <- function(x, ...){
  if (nrow(x) == 1) {
    scale <- -floor(log10(x[1,3]))
    val = round(x[1,3]*(10^scale),2)
    cat(
      "An study with ",
    x[1,1],
      " participants would have ",
    x[1,2]*100,
    "% power to detect at least one",
    " event if the true proportion of",
    " events is at least ",
    val,
    " per ",
    10^scale,
    " participants.",
    sep = "")
  } else {
      cat("According to the number of participants, ",
          "the table shows the power to detect at\n ",
          "least event given a true proportion of events or higher\n\n")
      cat("")
      subjects <- x[,1]
      powert <- paste0(x[,2]*100,"%")
      restext <- apply(
        x,
        1,
        function(y){
          scale <- -floor(log10(y[3]))
          val = round(y[3]*(10^scale),2)
          paste(
            formatC(val,format = "f", digits = 2),
            "per",
            10^scale,
            "participants")
        }
      )
      width1 = max(8, max(nchar(subjects)))
      width2 = max(5, max(nchar(powert)))
      width3 = max(10, max(nchar(restext)))
      cat("|",
          str_pad("Subjects", width1),
          "|",
          str_pad("Power", width2),
          "|",
          str_pad("Proportion", width3),
          "|\n", sep = "")

      cat(
          "|",
          paste0(rep("-",width1), collapse=""),
          "|",
          paste0(rep("-",width2), collapse=""),
          "|",
          paste0(rep("-",width3), collapse=""),
          "|\n",sep="")


      for (i in c(1:nrow(x))){
        cat(
          "|",
          str_pad(subjects[i],width1),
          "|",
          str_pad(powert[i], width2),
          "|",
          str_pad(restext[i],width3),
          "|\n", sep="")
      }
  }

}


# xx<- rate_power(30,0.9)
# xx
#
# yy<- rate_power(c(30,50,100), c(0.8))
# yy
#
