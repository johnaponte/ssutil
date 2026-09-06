# Calculate the Upper Equicoordinate Point of a Multivariate Normal Distribution

Computes the upper equicoordinate quantile for a multivariate standard
normal distribution with unit variances and a common correlation
coefficient `rho`. That is, it returns the value \\z\\ such that the
joint probability \\P(X_1 \le z, \ldots, X_n \le z) = 1 - \alpha\\.

## Usage

``` r
multz(alpha, k, rho, seed = NULL)
```

## Arguments

- alpha:

  Numeric. Significance level (e.g., 0.05 for a 95% confidence level).

- k:

  Integer. Number of variables in the multivariate normal distribution.
  Must be \>= 1.

- rho:

  Numeric. Common correlation coefficient between variables (typically
  between 0 and 1).

- seed:

  Optional. An object specifying if and how the random number generator
  should be initialized. Passed to
  [`qmvnorm`](https://rdrr.io/pkg/mvtnorm/man/qmvnorm.html).

## Value

Numeric. The upper equicoordinate point \\z\\ such that the joint
probability of all variables being less than or equal to \\z\\ is \\1 -
\alpha\\.

## Examples

``` r
alpha <- 0.1  # Significance level (10%)
k <- 3        # Number of variables
rho <- 0.5    # Common correlation coefficient
multz(alpha, k, rho)
#> [1] 1.734036
```
