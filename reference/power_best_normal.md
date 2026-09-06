# Power calculation for the Indifferent-zone approach for normal outcomes

Estimate the probability of correctly select the best group among
`ngroups` groups if the difference between the best group and the next
best is at least `dif`(the Indifferent-Zone), and the standard deviation
is `sd`

## Usage

``` r
power_best_normal(dif, sd, ngroups, npergroup, seed = NULL)
```

## Arguments

- dif:

  Numeric. Indifferent-zone. Minimum difference that is considered
  meaningful.

- sd:

  Numeric. Common standard deviation of the response variable.

- ngroups:

  Integer. Number of groups (treatments) being compared.

- npergroup:

  Integer. Number in each group.

- seed:

  Optional. Integer seed to use in the internal call to
  [`multp()`](https://johnaponte.github.io/ssutil/reference/multp.md).

## Value

Integer. Sample size required per group to achieve the specified power.

## Note

The function uses the quantile function
[`multp()`](https://johnaponte.github.io/ssutil/reference/multp.md),
which computes critical values for the selection procedure. This
implementation assumes equal variances and independent samples.

## Examples

``` r
power_best_normal(dif = 0.5, sd = 1, ngroups = 3, npergroup = 11)
#> [1] 0.801259
```
