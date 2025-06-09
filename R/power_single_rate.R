# Sample size for safety
# 20211101 by JJAV

#' Detectable Event Rate with Specified Power and Sample Size
#'
#' Estimates the minimum true proportion of events needed to detect at least one
#' event, given a sample size and desired statistical power.
#'
#' @param subjects Integer or vector of integers. Sample size(s).
#' @param power Numeric or vector of numerics. Desired power(s), between 0 and 1.
#'
##'
#' @return A matrix of class \code{power_single_rate} with columns:
#' \describe{
#'   \item{n}{Sample size}
#'   \item{power}{Requested power}
#'   \item{proportion}{Minimum detectable event rate to observe at least one event}
#' }
#'
#' @examples
#' power_single_rate(30, 0.9)
#' power_single_rate(c(30, 50, 100), 0.9)
#'
#' @importFrom stats dbinom uniroot
#' @importFrom stringr str_pad
#' @export
power_single_rate <- function(subjects, power) {
  res <- numeric()

  for (ni in subjects) {
    for (powerj in power) {
      stopifnot(as.integer(ni) == ni)
      stopifnot(0 < powerj, powerj < 1)
      ss <- uniroot(function(x) powerj - (1 - dbinom(0, ni, x)),
                    interval = c(0, 1))$root
      res <- c(res, ni, powerj, ss)
    }
  }

  rem <- matrix(res, ncol = 3, byrow = TRUE)
  colnames(rem) <- c("n", "power", "proportion")
  class(rem) <- c("power_single_rate", class(rem))
  rem
}

#' Format method for power_single_rate class
#' 
#' @return A character string with a human-readable summary of the detection
#'         power or a markdown-style table, depending on the number of rows.
#' @param x an R object of class power_single_rate  
#' @param digits  a positive integer indicating how many significant digits are
#'                to be used for numeric  x.
#' @param ... further arguments passed to or from other methods                       
#' @export
format.power_single_rate <- function(x, digits = 3, ...) {
  stopifnot(inherits(x, "power_single_rate"))

  scale <- -floor(log10(x[, 3]))
  val <- x[, 3] * (10^scale)
  valf <- format(val, digits = digits, ...)

  if (nrow(x) == 1) {
    res_str <- paste(
      "A study with ",
      x[1, 1], " participants would have ",
      x[1, 2] * 100, "% power to detect at least one event\n",
      "if the true event rate is at least ",
      valf, " per ", 10^scale, " participants.",
      sep = ""
    )
  } else {
    res_str <- paste(
      "According to the number of participants, the table shows the power\n",
      "to detect at least one event, given a true event rate equal to or higher than:\n\n",
      sep = ""
    )

    subjects <- x[, 1]
    powert <- paste0(x[, 2] * 100, "%")
    restext <- paste(valf, "per", 10^scale, "participants")

    width1 <- max(8, max(nchar(subjects)))
    width2 <- max(5, max(nchar(powert)))
    width3 <- max(10, max(nchar(restext)))

    res_str <- paste0(
      res_str,
      "| ", str_pad("Subjects", width1),
      " | ", str_pad("Power", width2),
      " | ", str_pad("Proportion", width3), " |\n",
      "| ", str_pad("", width1, pad = "-"),
      " | ", str_pad("", width2, pad = "-"),
      " | ", str_pad("", width3, pad = "-"), " |\n"
    )

    for (i in seq_len(nrow(x))) {
      res_str <- paste0(
        res_str,
        "| ", str_pad(subjects[i], width1),
        " | ", str_pad(powert[i], width2),
        " | ", str_pad(restext[i], width3), " |\n"
      )
    }
  }

  res_str
}


#' Print method for class power_single_rate
#' 
#' @param x an object of class power_single_rate
#' @param ... further arguments passed to or from other methods
#' @return Invisibly returns the object passed in.
#' @export
print.power_single_rate <- function(x, ...) {
  stopifnot(inherits(x, "power_single_rate"))
  cat(format(x, ...))
  invisible(x)
}
