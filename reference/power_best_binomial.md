# Power to Correctly Select the Best Group in a Binomial Test

Computes the exact probability of correctly identifying the best group
when the outcome follows a binomial distribution. It assumes that `p1`
is the probability of success in the best group, and that the success
probability in all other groups is lower by a fixed difference `dif`.

## Usage

``` r
power_best_binomial(p1, dif, ngroups, npergroup)
```

## Arguments

- p1:

  Numeric. Probability of success in the best group (must be in \[0,
  1\]).

- dif:

  Numeric. Difference in success probability between the best group and
  the next best (must be \> 0).

- ngroups:

  Integer. Number of groups (must be greater than 1).

- npergroup:

  Integer. Number of subjects per group (must be positive).

## Value

A numeric value representing the probability of correctly identifying
the best group.

## Details

The formula is based on the exact method described by Sobel and Huyett
(1957).

## References

Sobel, M., & Huyett, M. J. (1957). Selecting the Best One of Several
Binomial Populations. *Bell System Technical Journal*, 36(2), 537–576.
[doi:10.1002/j.1538-7305.1957.tb02411.x](https://doi.org/10.1002/j.1538-7305.1957.tb02411.x)

## Examples

``` r
power_best_binomial(p1 = 0.8, dif = 0.2, ngroups = 4, npergroup = 50)
#> [1] 0.963308
```
