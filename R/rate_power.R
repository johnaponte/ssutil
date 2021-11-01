# Sample size for safety
# 20211101 by JJAVE

#' Rate for a given power
#'
#' Estimate which is the true proportion required to observe
#' at least one event given a sample size and a power
#'
#'
#' @parm subjects sample size
#' @param power power
#' @return and s3 object of class rate_power with the proportion that can be
#' detected with the specified power and sample size
#' @export
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
    val = round(x[1,3]*(10^scale),1)
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
      subjects <- x[,1]
      powert <- paste(x[,2]*100,"%")
      restext <- apply(
        x,
        1,
        function(y){
          scale <- -floor(log10(y[3]))
          val = round(y[3]*(10^scale),1)
          return(paste(val, "per", 10^scale, "participants"))
        }
      )
  }
  cmatrix <- cbind(subjects, powert, restext)
  colnames(cmatrix)<-c("Subjects","Power","Proportion")
  print(cmatrix)
}


xx<- rate_power(30,0.9)
xx
yy<- rate_power(c(30,50,220), c(0.8,0.9))
yy
