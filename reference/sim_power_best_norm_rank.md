# Simulate Power to Select Best Group by Ranks (Normal Outcomes)

Estimates the empirical power to identify the most promising group as
best, using weighted ranks across outcomes, assuming normally
distributed outcomes.

## Usage

``` r
sim_power_best_norm_rank(
  noutcomes,
  sd,
  dif,
  weights,
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

  Numeric vector. Standard deviations for each outcome.

- dif:

  Numeric vector. Difference in means between the best and other groups.

- weights:

  Numeric vector. Weights per outcome.

- ngroups:

  Integer. Number of groups.

- npergroup:

  Integer or vector. Number of subjects per group.

- nsim:

  Integer. Number of simulations.

- conf.level:

  Numeric. Confidence level for the empirical power estimate

## Value

An S3 object of class `empirical_power_result`, which contains the
estimated empirical power and its confidence interval. The object can be
printed, formatted, or further processed using associated S3 methods.
See also
[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md).

## Details

Each outcome is independent and normally distributed. The most promising
group is assumed to have a mean at least `dif` higher than the others.
Ranks are weighted and summed per group across outcomes.

If `weights` is specified, it is internally scaled to sum to 1. The most
promising group is always considered to be the first group.

## See also

[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md)

## Examples

``` r
sim_power_best_norm_rank(
  noutcomes = 3,
  sd = c(1, 0.8, 1.5),
  dif = c(0.2, 0.15, 0.3),
  weights = c(0.5, 0.3, 0.2),
  ngroups = 3,
  npergroup = c(30, 25, 25),
  nsim = 1000
)
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.7370
#> 95% CI:      [0.7085, 0.7641]
#> Simulations: 1000
```
