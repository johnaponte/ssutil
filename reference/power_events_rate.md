# Probability of Observing At Least a Given Number of Events

Computes the exact binomial probability of observing at least `e`
events, for every combination of sample size and risk, and for one or
more event thresholds.

## Usage

``` r
power_events_rate(n, r, e)
```

## Arguments

- n:

  Integer or vector of integers. Sample size(s).

- r:

  Numeric or vector of numerics. Risk(s) (per-subject event
  probability), between 0 and 1.

- e:

  Integer or vector of integers. Event count threshold(s), e.g. 1, 2, 3.

## Value

A matrix of class `power_events_rate` with columns:

- N:

  Sample size

- Risk:

  Per-subject event probability

- \>= e:

  One column per threshold in `e`, named using the "greater than or
  equal to" symbol followed by the threshold value, giving P(X \>= e)
  for X ~ Binomial(N, Risk)

## Examples

``` r
power_events_rate(30, 0.1, 1:3)
#> |  N | Risk |    ≥1 |    ≥2 |    ≥3 |
#> | -- | ---- | ----- | ----- | ----- |
#> | 30 | 1/10 | 95.8% | 81.6% | 58.9% |
power_events_rate(c(30, 60), c(0.05, 0.1), c(1, 2, 3))
#> |  N | Risk |    ≥1 |    ≥2 |    ≥3 |
#> | -- | ---- | ----- | ----- | ----- |
#> | 30 | 1/20 | 78.5% | 44.6% | 18.8% |
#> | 30 | 1/10 | 95.8% | 81.6% | 58.9% |
#> | 60 | 1/20 | 95.4% | 80.8% | 58.3% |
#> | 60 | 1/10 | 99.8% | 98.6% | 94.7% |
```
