# Calculate the Upper Equicoordinate Point of a Multivariate Normal Distribution

Computes the upper equicoordinate quantile for a multivariate standard
normal distribution with unit variances and a common correlation
coefficient `rho`. That is, it returns the value \\z\\ such that the
joint probability \\P(X_1 \le z, \ldots, X_n \le z) = p\\.

## Usage

``` r
multz(p, k, rho, lower.tail = TRUE, seed = NULL)
```

## Arguments

- p:

  Numeric. Cumulative probability (e.g., 0.95 for the 95th

- k:

  Integer. Number of variables in the multivariate normal distribution.
  Must be \>= 1.

- rho:

  Numeric. Common correlation coefficient between variables (typically
  between 0 and 1).

- lower.tail:

  Logical. If `TRUE` (default), probability is \\P(X \le z)\\; if
  `FALSE`, \\P(X \> z)\\

- seed:

  Optional. An object specifying if and how the random number generator
  should be initialized. Passed to
  [`qmvnorm`](https://rdrr.io/pkg/mvtnorm/man/qmvnorm.html).

## Value

Numeric. The equicoordinate quantile \\z\\

## Examples

``` r
p <- 0.9    # Significance level (10%)
k <- 3      # Number of variables
rho <- 0.5  # Common correlation coefficient
multz(p, k, rho)
#> [1] 1.734036
```
