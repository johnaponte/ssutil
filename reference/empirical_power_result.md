# Create an Empirical Power Result object

Constructs an S3 object of class `empirical_power_result`, storing the
estimated power, its confidence interval, and the number of simulations
used to compute it.

## Usage

``` r
empirical_power_result(x, n, conf.level = 0.95)
```

## Arguments

- x:

  Number of successes

- n:

  Number of trials.

- conf.level:

  Confidence level for the returned confidence interval power.

## Value

An object of class `empirical_power_result`, a list with components:

- `power`: Estimated power.

- `conf.low`: Lower bound of confidence interval.

- `conf.high`: Upper bound of confidence interval.

- `conf.level`: Confidence level for the returned confidence interval.

- `nsim`: Number of simulations.

## Details

It is a wrap to [`binom.test`](https://rdrr.io/r/stats/binom.test.html)

## Examples

``` r
result <- empirical_power_result(
  x = 10,
  n = 100,
  conf.level = 0.95
)
print(result)
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.1000
#> 95% CI:      [0.0490, 0.1762]
#> Simulations: 100
```
