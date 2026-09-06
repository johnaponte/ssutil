# Equivalence Testing for Normal Outcomes

``` r

library(ssutil)
```

## Introduction

This vignette illustrates how the
[`sim_power_equivalence_normal()`](https://johnaponte.github.io/ssutil/reference/sim_power_equivalence_normal.md)
function can be used to estimate the empirical power of an equivalence
test under the assumption of normally distributed outcomes. The method
simulates repeated trials and determines whether all pairwise confidence
intervals for differences in group means fall within user-specified
equivalence limits.

## Example: Equivalence Among Vaccine Lots

An evaluation is conducted to determine whether three manufacturing lots
of a vaccine produce equivalent immune responses. The outcome is the
antibody concentration measured on the $`\log_{10}`$ scale, assumed to
follow a normal distribution with a standard deviation of 0.4.
Equivalence is declared if the confidence intervals for the ratio of all
pairwise comparisons fall entirely within the range \[2/3, 3/2\].

Since the analysis is conducted on the $`\log_{10}`$ scale, the
equivalence limits are transformed to `log10(2/3)` and `log10(3/2)`.

A total of 1,000 trials are simulated with 172 subjects per group and a
95% confidence level:

``` r

set.seed(12345)
sim_power_equivalence_normal(
  ngroups = 3,
  npergroup = 172,
  sd = 0.4,
  llimit = log10(2/3),
  ulimit = log10(3/2),
  nsim = 1000,
  t_level = 0.95
)
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.9130
#> 95% CI:      [0.8938, 0.9297]
#> Simulations: 1000
```

The result shows the proportion of simulations in which all pairwise
comparisons satisfy the equivalence criterion.

A power calculation in nQuery® shows that a Two One-Sided Equivalence
Test (TOST) for a two-group design, with an alpha level of 0.025, 172
participants per group, a standard deviation of 0.4, and equivalence
limits of `log10(2/3)` and `log10(3/2)`, has a power of 96.98%. When
extended to three comparisons, assuming independence, the joint
probability of all three satisfying the equivalence condition is
approximately: $`96.98\%^3 = 91.21\%`$.
