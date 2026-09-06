# Simulations for Negative Binomial outcomes

``` r

library(ssutil)
```

## Introduction

The `sim_power_nbinom` function is used to estimate the empirical power
of a negative binomial regression model. This method is useful when
outcomes consist of overdispersed count data, and the objective is to
test whether the relative risk (RR) between two groups differs
significantly from a null boundary.

Data are simulated under a negative binomial distribution, a model is
fit using `glm.nb`, and it is determined whether the null hypothesis is
rejected based on confidence interval limits relative to a specified
boundary. The procedure accounts for whether the true RR is below or
above 1, applying the appropriate directional test logic.

## Example

What is the empirical power, using negative binomial regression, to
detect a vaccine efficacy of 60% when the lower bound of a 95%
confidence interval must exceed a boundary of 30%, if the sample size is
500 per group, the average follow-up is 1 year, the event rate in the
control group is 0.2 episodes per person-year, and the dispersion
parameter is 2?

``` r

library(ssutil)
set.seed(123)
result <- sim_power_nbinom(
  n1 = 500, n2 = 500,
  ir1 = 0.2, tm = 1,
  rr = 0.4, boundary = 0.7,
  dispersion = 2,
  alpha = 0.05,
  nsim = 1000
)

result
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.7990
#> 95% CI:      [0.7728, 0.8234]
#> Simulations: 1000
```
