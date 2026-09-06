# Detectable Event Rate with Specified Power and Sample Size

Estimates the minimum true proportion of events needed to detect at
least one event, given a sample size and desired statistical power.

## Usage

``` r
power_single_rate(subjects, power)
```

## Arguments

- subjects:

  Integer or vector of integers. Sample size(s).

- power:

  Numeric or vector of numerics. Desired power(s), between 0 and 1.

## Value

A matrix of class `power_single_rate` with columns:

- n:

  Sample size

- power:

  Requested power

- proportion:

  Minimum detectable event rate to observe at least one event

## Examples

``` r
power_single_rate(30, 0.9)
#> A study with 30 participants would have 90% power to detect at least one event
#> if the true event rate is at least 7.39 per 100 participants.
power_single_rate(c(30, 50, 100), 0.9)
#> According to the number of participants, the table shows the power
#> to detect at least one event, given a true event rate equal to or higher than:
#> 
#> | Subjects | Power |                Proportion |
#> | -------- | ----- | ------------------------- |
#> |       30 |   90% | 7.39 per 100 participants |
#> |       50 |   90% | 4.50 per 100 participants |
#> |      100 |   90% | 2.28 per 100 participants |
```
