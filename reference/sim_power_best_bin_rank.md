# Simulate Power to Rank the Best Group Using Binomial Outcomes

Estimates the empirical power to rank the most promising group as the
best, based on binomial outcomes, via simulation.

## Usage

``` r
sim_power_best_bin_rank(
  noutcomes,
  p1,
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

- p1:

  Numeric. Event probability in the best group (scalar or vector of
  length `noutcomes`).

- dif:

  Numeric. Difference between the best group and the rest (scalar or
  vector of length `noutcomes`).

- weights:

  Numeric vector. Weights for each outcome. If scalar, applied equally.

- ngroups:

  Integer. Number of groups.

- npergroup:

  Integer or vector. Sample size per group.

- nsim:

  Integer. Number of simulations.

- conf.level:

  Numeric. Confidence level for the empirical power estimate#'

## Value

An S3 object of class `empirical_power_result`, which contains the
estimated empirical power and its confidence interval. The object can be
printed, formatted, or further processed using associated S3 methods.
See also
[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md).

## Details

Each outcome is assumed to follow an independent binomial distribution.
The best group is defined as having a probability at least `dif` higher
than the other groups. The function sums weighted ranks across multiple
outcomes to determine the top group.

If multiple outcomes are defined, weights can be applied to prioritize
some outcomes over others. Weights are automatically scaled to sum 1.
The group with the lowest total rank is considered the best.

## See also

[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md)

## Examples

``` r
  sim_power_best_bin_rank(
  noutcomes = 2,
  p1 = 0.80,
  dif = 0.15,
  weights = 1,
  ngroups = 3,
  npergroup = 30,
  nsim = 1000,
  conf.level = 0.95)
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.8820
#> 95% CI:      [0.8604, 0.9013]
#> Simulations: 1000
```
