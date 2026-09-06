# Tidy Method for empirical_power_result

Creates a one-row tibble with the power estimate and confidence
interval.

## Usage

``` r
# S3 method for class 'empirical_power_result'
tidy(x, ...)
```

## Arguments

- x:

  A `empirical_power_result` object.

- ...:

  Ignored.

## Value

A tibble with columns: `power`, `conf.low`, `conf.high`, `conf.level`.
`nsim`.
