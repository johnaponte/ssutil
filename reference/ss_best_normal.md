# Sample Size for Selecting the Best Treatment in a Normal Response (Indifference-Zone)

Calculates the minimum common sample size per group needed to achieve a
specified probability (power) of correctly selecting the best group
using the indifference-zone approach. This method assumes normally
distributed responses with a known and common standard deviation.

## Usage

``` r
ss_best_normal(power, dif, sd, ngroups, seed = NULL)
```

## Arguments

- power:

  Numeric. Desired probability of correctly selecting the best group.

- dif:

  Numeric. Indifferent-zone. Minimum difference that is considered
  meaningful.

- sd:

  Numeric. Common standard deviation of the response variable.

- ngroups:

  Integer. Number of groups (treatments) being compared.

- seed:

  Optional. Integer seed to use in the internal call to
  [`multz()`](https://johnaponte.github.io/ssutil/reference/multz.md).

## Value

Integer. Sample size required per group to achieve the specified power.

## Details

The indifference-zone approach guarantees that the probability of
correct selection is at least `power`, assuming the best group's mean
exceeds the others by at least `dif`. The calculation is based on
Bechhofer's Procedure Nb.

## Note

The function uses the quantile function
[`multz()`](https://johnaponte.github.io/ssutil/reference/multz.md),
which computes critical values for the selection procedure. This
implementation assumes equal variances and independent samples.

## References

Bechhofer, R.E., Santner, T.J., & Goldsman, D.M. (1995). *Design and
Analysis of Experiments for Statistical Selection, Screening, and
Multiple Comparisons.* Wiley Series in Probability and Statistics. ISBN:
0-471-57427-9.

## Examples

``` r
ss_best_normal( power = 0.8, dif = 0.5, sd = 1, ngroups = 3)
#> [1] 10.92462
```
