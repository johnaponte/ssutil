#' Probability of Observing At Least a Given Number of Events
#'
#' Computes the exact binomial probability of observing at least \code{e}
#' events, for every combination of sample size and risk, and for one or more
#' event thresholds.
#'
#' @param n Integer or vector of integers. Sample size(s).
#' @param r Numeric or vector of numerics. Risk(s) (per-subject event
#'   probability), between 0 and 1.
#' @param e Integer or vector of integers. Event count threshold(s), e.g. 1, 2, 3.
#'
#' @return A matrix of class \code{power_events_rate} with columns:
#' \describe{
#'   \item{N}{Sample size}
#'   \item{Risk}{Per-subject event probability}
#'   \item{≥e}{One column per threshold in \code{e}, giving
#'     P(X >= e) for X ~ Binomial(N, Risk)}
#' }
#'
#' @examples
#' power_events_rate(30, 0.1, 1:3)
#' power_events_rate(c(30, 60), c(0.05, 0.1), c(1, 2, 3))
#'
#' @importFrom stats pbinom
#' @export
power_events_rate <- function(n, r, e) {
  stopifnot("n must be positive integers" = all(n == as.integer(n) & n > 0))
  stopifnot("r must be between 0 and 1" = all(r >= 0 & r <= 1))
  stopifnot("e must be positive integers" = all(e == as.integer(e) & e > 0))

  grid <- expand.grid(Risk = r, N = n)[, c("N", "Risk")] |> as.matrix()

  atleast <- function(k) 1 - pbinom(k - 1, grid[, "N"], grid[, "Risk"])
  probs <- vapply(e, atleast, numeric(nrow(grid)))
  probs <- matrix(probs, nrow = nrow(grid), dimnames = list(NULL, paste0("\u2265", e)))

  rem <- cbind(grid, probs)
  class(rem) <- c("power_events_rate", class(rem))
  rem
}

#' Format method for power_events_rate class
#'
#' @return A character string with a markdown-style table of the
#'   probabilities of observing each event threshold, by sample size and risk.
#' @param x an R object of class power_events_rate
#' @param digits a positive integer indicating how many decimal digits are
#'               to be used to display the probability columns as percentages.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_pad
#' @export
format.power_events_rate <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "power_events_rate"))

  cols <- colnames(x)
  format_col <- function(j) {
    if (cols[j] == "Risk") paste0("1/", formatC(round(1 / x[, j]), format = "d"))
    else if (cols[j] == "N") formatC(x[, j], format = "d")
    else paste0(formatC(100 * x[, j], format = "f", digits = digits), "%")
  }
  cells <- vapply(seq_along(cols), format_col, character(nrow(x)))
  cells <- matrix(cells, nrow = nrow(x), dimnames = list(NULL, cols))

  widths <- pmax(nchar(cols), apply(cells, 2, function(col) max(nchar(col))))

  row_line <- function(values) {
    paste0("| ", paste(str_pad(values, widths), collapse = " | "), " |\n")
  }

  body <- paste(
    vapply(seq_len(nrow(x)), function(i) row_line(cells[i, ]), character(1)),
    collapse = ""
  )

  paste0(
    row_line(cols),
    row_line(strrep("-", widths)),
    body
  )
}

#' Print method for class power_events_rate
#'
#' @param x an object of class power_events_rate
#' @param ... further arguments passed to or from other methods
#' @return Invisibly returns the object passed in.
#' @export
print.power_events_rate <- function(x, ...) {
  stopifnot(inherits(x, "power_events_rate"))
  cat(format(x, ...))
  invisible(x)
}
