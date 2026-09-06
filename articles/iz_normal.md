# Selecting the best group using the Indifference-Zone approach for normal outcomes

``` r

library(ssutil)
```

## Introduction

The indifference-zone approach for normal outcomes is a statistical
method designed to select the group with the highest mean while ensuring
that this selection is made correctly at a specified confidence level.
This approach assumes that the difference in means between the best
group and the next-best group exceeds a specified threshold, called the
“indifference zone”. This zone defines a margin of indifference, within
which differences are considered negligible, allowing the decision
process to focus only on differences that clearly exceed this margin.

The procedures presented are based on a single-stage selection of an
outcome with a known standard deviation. If the standard deviation is
not known, a multiple-stage approach is recommended, or assume that the
true standard deviation is not larger than the specified standard
deviation. The power will be higher if the true standard deviation is
lower.

This package offers several functions to help with this design:

[`power_best_normal()`](https://johnaponte.github.io/ssutil/reference/power_best_normal.md)
calculates for an outcome with known standard deviation (`sd`) the
probability of correctly selecting the best group in a single stage,
given the pre-specified indifference-zone threshold (`dif`), the number
of groups (`ngroups`), and the sample size per group (`npergroup`).

[`ss_best_normal()`](https://johnaponte.github.io/ssutil/reference/ss_best_normal.md)
estimates the required sample size per group to achieve a specified
power for correctly selecting in a single stage the best group, given
the known standard deviation (`sd`), the indifference-zone threshold
(`dif`), and the number of groups (`ngroups`). This function is based on
the procedure $`\mathscr{N}b`$ from Bechhofer et al. (1995).

[`sim_power_best_normal()`](https://johnaponte.github.io/ssutil/reference/sim_power_best_normal.md)
estimates the empirical power (i.e., the proportion of simulated trials
in which the best group is correctly identified) via Monte Carlo
simulation. It supports multiple outcomes and can estimate the empirical
power to select the true best group across all outcomes.

[`sim_power_best_norm_rank()`](https://johnaponte.github.io/ssutil/reference/sim_power_best_norm_rank.md)
is similar to
[`sim_power_best_normal()`](https://johnaponte.github.io/ssutil/reference/sim_power_best_normal.md),
but it defines the best group based on overall ranking across multiple
outcomes rather than requiring top performance on every outcome.

## Examples with a single outcome

1.  What is the probability of correctly selecting in a single stage the
    best group in a trial with three groups of 30 participants each?
    Assume the outcome has a standard deviation of 0.5 and the
    indifference-zone threshold is 0.10.

``` r

power_best_normal(sd = 0.5, dif = 0.10, ngroups = 3, npergroup = 30)
#> [1] 0.660936
```

2.  What is the sample size required per group to achieve 80% power for
    correctly selecting in a single stage the best group among three
    groups, assuming the outcome has a known standard deviation of 0.5
    and the indifference-zone threshold is 0.10.

``` r

ss_best_normal(power = 0.8, sd = 0.5, dif = 0.1, ngroups = 3)
#> [1] 68.27888
```

3.  Using simulations, what is the probability of correctly selecting
    the best group in a trial with three groups of 30 participants each?
    Assume the standard deviation is 0.5 and the indifference-zone
    threshold is 0.1.

``` r

set.seed(1234)
sim_power_best_normal(
  noutcomes = 1,
  sd = 0.5,
  dif = 0.1,
  ngroups = 3,
  npergroup = 30,
  nsim = 1000
)
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.6640
#> 95% CI:      [0.6338, 0.6933]
#> Simulations: 1000
```

## Examples using multiple outcomes

The
[`sim_power_best_normal()`](https://johnaponte.github.io/ssutil/reference/sim_power_best_normal.md)
and
[`sim_power_best_norm_rank()`](https://johnaponte.github.io/ssutil/reference/sim_power_best_norm_rank.md)
allow simulating multiple outcomes. These functions differ in how they
define the ‘best’ group.
[`sim_power_best_normal()`](https://johnaponte.github.io/ssutil/reference/sim_power_best_normal.md)
requires that the best group be the top performer for every outcome,
whereas
[`sim_power_best_norm_rank()`](https://johnaponte.github.io/ssutil/reference/sim_power_best_norm_rank.md)
defines the best group based on overall ranking across outcomes. For
example, a group might rank first for the first two outcomes but second
for the third, yet still achieve the best overall rank among all groups.
Both procedures assume the multiple outcomes are independent between
them.

This ranking approach supports weighting of outcomes, allowing you to
assign greater importance to some outcomes over others. For instance, if
performance on the first two outcomes is twice as important as the
third, you could specify weights of `c(0.4, 0.4, 0.2)`. Weights are
scaled internally to sum to 1.

The functions are flexible and allow you to specify, for each outcome,
the event probabilities, indifference-zone thresholds, and group sample
sizes.

1.  What is the probability that the best group is correctly identified
    as having the highest antibody titer across five antigens in a trial
    with three groups of 30 participants each? The standard deviation is
    not known but assume it is not greater than 0.5, and the
    indifference-zone threshold is 0.1 for all outcomes.

``` r

set.seed(12345)
sim_power_best_normal(
  noutcomes = 5,
  sd = 0.5,
  dif = 0.10,
  ngroups = 3,
  npergroup = 30,
  nsim = 1000
)
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.1150
#> 95% CI:      [0.0959, 0.1364]
#> Simulations: 1000
```

2.  Same setup, but define the best group based on overall ranking
    across the five outcomes with equal weights.

``` r

set.seed(12345)
sim_power_best_norm_rank(
  noutcomes = 5,
  sd = 0.5,
  dif = 0.10,
  weights = 1,
  ngroups = 3,
  npergroup = 30,
  nsim = 1000
)
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.8700
#> 95% CI:      [0.8476, 0.8902]
#> Simulations: 1000
```

## References

Bechhofer, R. E., Santner, T. J., & Goldsman, D. M. (1995). Design and
analysis of experiments for statistical selection, screening, and
multiple comparisons. Wiley. ISBN 978-0-471-57427-9
