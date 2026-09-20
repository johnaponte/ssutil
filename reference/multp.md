# Multivariate Normal Equicoordinate Cumulative Distribution Function

Computes the joint CDF for a multivariate standard normal distribution
with unit variances and a common correlation coefficient `rho`. It is
the exact functional inverse of `multz`:
`multp(multz(q, k, rho), k, rho) == q`.

## Usage

``` r
multp(q, k, rho, lower.tail = TRUE, seed = NULL)
```

## Arguments

- q:

  Numeric. Quantile of the distribution.

- k:

  Integer. Number of variables in the multivariate normal distribution.
  Must be \>= 1.

- rho:

  Numeric. Common correlation coefficient between variables (typically
  between 0 and 1).

- lower.tail:

  Logical. If `TRUE` (default), probability is \\P(X \le q)\\; if
  `FALSE`, \\P(X \> q)\\. Mirrors
  [`pnorm`](https://rdrr.io/r/stats/Normal.html).

- seed:

  Optional. An object specifying if and how the random number generator
  should be initialized. Passed to
  [`pmvnorm`](https://rdrr.io/pkg/mvtnorm/man/pmvnorm.html).

## Value

Numeric. The joint cumulative probability.

## Examples

``` r
q <- 1.3      
k <- 3        
rho <- 0.5    
multp(q, k, rho)
#> [1] 0.7876844
```
