# Empirical Power for Negative Binomial Comparison

Estimates empirical power to detect a relative risk either above or
below a specified boundary, depending on the direction of the
alternative hypothesis. Simulates count data with over dispersion, fits
a model with `glm.nb`, and evaluates the power to reject the null
hypothesis using a negative binomial model.

## Usage

``` r
sim_power_nbinom(
  n1,
  n2,
  ir1,
  tm,
  rr,
  boundary,
  dispersion,
  alpha,
  nsim,
  conf.level = 0.95
)
```

## Arguments

- n1:

  Integer. Number of participants in group 1.

- n2:

  Integer. Number of participants in group 2.

- ir1:

  Numeric. Incidence rate in group 1.

- tm:

  Numeric. Average exposure time per subject (assumed equal across
  subjects).

- rr:

  Numeric. True relative risk between groups (group 2 rate = rr × group
  1 rate).

- boundary:

  Numeric. Relative risk boundary under the null hypothesis.

- dispersion:

  Numeric. Dispersion parameter (\\\phi\\) for the negative binomial
  distribution.

- alpha:

  Numeric. Type I error rate (two-sided).

- nsim:

  Integer. Number of simulation iterations.

- conf.level:

  Numeric. Confidence level for the empirical power estimate

## Value

An S3 object of class `empirical_power_result`, which contains the
estimated empirical power and its confidence interval. The object can be
printed, formatted, or further processed using associated S3 methods.
See also
[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md).

## Note

Uses the alternative parameterization of the negative binomial: `mu` is
the mean, and `size = 1/dispersion`. In `glm.nb`, dispersion is
estimated as `theta`. The 'boundary' parameter defines the relative risk
under the null hypothesis. When rr \< 1, rejection occurs if the upper
limit of the confidence interval is below the boundary. When rr \> 1,
rejection occurs if the lower limit is above the boundary.

The `alpha` parameter is two-sided as it is used to estimate two-sided
confidence intervals

## See also

[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md)

## Author

Chris Gast

John J. Aponte

## Examples

``` r
# \donttest{
sim_power_nbinom(
 n1 = 150, n2 = 150,
 ir1 = 0.55, tm = 1.7,
 rr = 0.6, boundary = 1,
 dispersion = 2,
 alpha = 0.05,
 nsim = 1000
)
#> Empirical Power Result
#> ----------------------- 
#> Power:       0.6960
#> 95% CI:      [0.6664, 0.7244]
#> Simulations: 1000
# }
```
