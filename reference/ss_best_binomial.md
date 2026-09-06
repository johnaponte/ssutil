# Sample Size to Select the Best Group in a Binomial Test

Computes the minimum sample size per group required to achieve a target
probability of correctly selecting the best group in a binomial test.
The best group is assumed to have success probability `p1`, and the
other groups have `p1 - dif`.

## Usage

``` r
ss_best_binomial(power, p1, dif, ngroups, max_n = 1000)
```

## Arguments

- power:

  Numeric. Desired probability of correctly selecting the best group (in
  \[0, 1\]).

- p1:

  Numeric. Probability of success in the best group (in \[0, 1\]).

- dif:

  Numeric. Difference in success probability with the next best group
  (\> 0).

- ngroups:

  Integer. Number of groups (must be \> 1).

- max_n:

  Integer. Maximum sample size to evaluate (default is 1000).

## Value

An integer representing the minimum sample size per group required to
reach the specified power.

## Details

The function searches for the smallest `npergroup` such that the power
from
[`power_best_binomial`](https://johnaponte.github.io/ssutil/reference/power_best_binomial.md)
is at least the target `power`.

## Examples

``` r
ss_best_binomial(power = 0.9, p1 = 0.8, dif = 0.2, ngroups = 4)
#> [1] 33
```
