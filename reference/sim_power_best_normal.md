# Simulate Power to Select Best Group (Normal Outcomes)

Estimates the empirical power to identify the most promising group as
the best, when outcomes are normally distributed and independent.

## Usage

``` r
sim_power_best_normal(
  noutcomes,
  sd,
  dif,
  ngroups,
  npergroup,
  nsim,
  conf.level = 0.95
)
```

## Arguments

- noutcomes:

  Integer. Number of outcomes to evaluate.

- sd:

  Numeric vector. Standard deviations for each outcome. Can be a single
  value.

- dif:

  Numeric vector. Difference in means between the best and the other
  groups.

- ngroups:

  Number of groups to compare.

- npergroup:

  Number of subjects per group. Can be scalar or vector of length
  `ngroups`.

- nsim:

  Integer. Number of simulations to perform.

- conf.level:

  Numeric. Confidence level for the empirical power estimate

## Value

An S3 object of class `empirical_power_result`, which contains the
estimated empirical power and its confidence interval. The object can be
printed, formatted, or further processed using associated S3 methods.
See also
[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md).

## Details

The best group (group 1) is assumed to have mean 0, and the rest of the
groups have mean `-dif`.

Multiple outcomes can be evaluated simultaneously. The power is
estimated as the proportion of simulations where the most promising
group is the best in all outcomes.

The number of subjects per group can be the same or specified per group.
In either case, the first group is assumed to be the most promising.

## See also

[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md)

## Examples

``` r
  sim_power_best_normal(
   noutcomes = 2,
   sd = c(1, 1.2),
   dif = c(0.2, 0.25),
   ngroups = 3,
   npergroup = c(30, 25, 25),
   nsim = 1000
  )
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.4300
#> 95% CI:      [0.3991, 0.4614]
#> Simulations: 1000
```
