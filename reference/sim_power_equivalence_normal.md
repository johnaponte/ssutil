# Empirical Power for Equivalence (Normal Outcomes)

Estimates the empirical power to detect equivalence among multiple
groups assuming no true difference in normally distributed outcomes.
Pairwise two-sample t-tests are used, and equivalence is declared if all
confidence intervals for differences between group means lie entirely
within the interval defined by `llimit` and `ulimit`.

## Usage

``` r
sim_power_equivalence_normal(
  ngroups,
  npergroup,
  sd,
  llimit,
  ulimit,
  nsim,
  t_level = 0.95,
  conf.level = 0.95
)
```

## Arguments

- ngroups:

  Integer. Number of groups to compare

- npergroup:

  Integer. Number of observations per group.

- sd:

  Numeric. Standard deviation of the outcome distribution (common across
  groups).

- llimit:

  Numeric. Lower equivalence limit.

- ulimit:

  Numeric. Upper equivalence limit.

- nsim:

  Integer. Number of simulations to perform.

- t_level:

  Numeric. Confidence level used for the t-tests (e.g., 0.95 for 95%
  CI).

- conf.level:

  Numeric. Confidence level for the empirical power estimate

## Value

An S3 object of class `empirical_power_result`, which contains the
estimated empirical power and its confidence interval. The object can be
printed, formatted, or further processed using associated S3 methods.
See also
[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md).

## Details

This function simulates data under the null hypothesis of no difference
between groups and calculates the proportion of simulations in which all
pairwise comparisons fall within the specified equivalence limits.

## See also

[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md)

## Examples

``` r
#Equivalence testing for three groups with log-scale outcome
sim_power_equivalence_normal(
  ngroups = 3,
  npergroup = 172,
  sd = 0.403,
  llimit = log10(2/3),
  ulimit = log10(3/2),
  nsim = 1000,
  t_level = 0.95
)
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.9160
#> 95% CI:      [0.8971, 0.9324]
#> Simulations: 1000
```
