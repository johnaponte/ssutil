# Calculate the Multivariate Normal Probability

Computes the multivariate normal probabilities with arbitrary
correlation matrices It is the inverse of the `multz` function

## Usage

``` r
multp(q, k, rho, seed = NULL)
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

- seed:

  Optional. An object specifying if and how the random number generator
  should be initialized. Passed to
  [`pmvnorm`](https://rdrr.io/pkg/mvtnorm/man/pmvnorm.html).

## Value

Numeric. The multivariate probability

## Examples

``` r
q <- 1.3      
k <- 3        
rho <- 0.5    
multp(q, k, rho)
#> [1] 0.2123156
```
