# Worst‐Case Scenario Power for the Best Binomial Group

Searches for the probability in the best‐performing group that yields
the lowest statistical power, given an indifference zone specification,
a number of groups, and a number of subjects per group.

## Usage

``` r
wcs_power_best_binomial(dif, ngroups, npergroup)
```

## Arguments

- dif:

  Numeric. Indifference zone specification (difference threshold).

- ngroups:

  Integer. Number of groups to compare.

- npergroup:

  Integer. Number of subjects per group.

## Value

A named list with components:

- p1:

  Numeric. Probability in the best group that yields the minimum power.

- minimum_power:

  Numeric. The minimum power achieved at `p1`.

## Details

Defines an internal function `fx` that wraps
[`power_best_binomial`](https://johnaponte.github.io/ssutil/reference/power_best_binomial.md)
with the supplied parameters, then uses
[`optimize`](https://rdrr.io/r/stats/optimize.html) over the interval
\[0,1\] to find the probability `p1` that minimizes the resulting power.

## See also

[`power_best_binomial`](https://johnaponte.github.io/ssutil/reference/power_best_binomial.md),
[`optimize`](https://rdrr.io/r/stats/optimize.html)

## Examples

``` r
wcs_power_best_binomial(dif = 0.1, ngroups = 3, npergroup = 50)
#> $p1
#> [1] 0.5541703
#> 
#> $minimum_power
#> [1] 0.7410576
#> 
```
