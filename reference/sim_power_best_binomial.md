# Simulate Power to Select the Best Group Using Binomial Outcomes

Estimates the empirical power to correctly identify the best group as
having the highest outcome, under a binomial distribution. Assumes that
the most promising group has a higher success probability than the
others by at least `dif`, and that outcomes are independent.

## Usage

``` r
sim_power_best_binomial(
  noutcomes,
  p1,
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

- p1:

  Numeric. Probability in the most promising group (scalar or vector).

- dif:

  Numeric. Difference between the best group and the rest.

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

Multiple outcomes can be evaluated simultaneously. The power is
estimated as the proportion of simulations where the most promising
group is selected best in all outcomes.

## See also

[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md)

## Examples

``` r
sim_power_best_binomial(
  noutcomes = 1,
  p1 = 0.7,
  dif = 0.2,
  ngroups = 3,
  npergroup = 30,
  nsim = 1000
)
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.9050
#> 95% CI:      [0.8851, 0.9225]
#> Simulations: 1000
```
