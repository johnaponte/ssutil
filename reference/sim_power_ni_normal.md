# Empirical Power for Non-Inferiority (Normal Outcomes)

Estimates empirical power to declare non-inferiority between two groups
across multiple outcomes using t-tests. Simulates normally distributed
data under the null (no difference) and applies non-inferiority rules
based on user-defined required and optional tests.

## Usage

``` r
sim_power_ni_normal(
  nsim,
  npergroup,
  ntest,
  ni_limit,
  test_req,
  test_opt,
  sd,
  corr = 0,
  t_level = 0.95,
  conf.level = 0.95
)
```

## Arguments

- nsim:

  Integer. Number of simulations to perform.

- npergroup:

  Integer. Number of observations per group.

- ntest:

  Integer. Number of tests (outcomes) to compare.

- ni_limit:

  Numeric. Limit to declare non-inferiority. Can be a scalar or vector
  of length `ntest`.

- test_req:

  Integer. Number of required tests that must show non-inferiority
  (first `test_req` tests).

- test_opt:

  Integer. Number of optional tests that must also show non-inferiority
  from the remaining tests.

- sd:

  Numeric. Standard deviation(s) of the outcomes. Scalar or vector of
  length `ntest`.

- corr:

  Numeric. Correlation between the tests. Scalar (common correlation),
  or vector of length `ntest*(ntest-1)/2`.

- t_level:

  Numeric. Confidence level used for the t-tests (e.g., 0.95 for 95%
  CI).scalar or vector of length `ntest`.

- conf.level:

  Numeric. Confidence level for the empirical power estimate

## Value

An S3 object of class `empirical_power_result`, which contains the
estimated empirical power and its confidence interval. The object can be
printed, formatted, or further processed using associated S3 methods.
See also
[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md).

## Details

A test is considered non-inferior if the lower bound of its confidence
interval is greater than the specified non-inferiority limit. Overall
non-inferiority is declared if all `test_req` and at least `test_opt` of
the remaining tests are non-inferior.

## Note

If only one test is used, correlation is ignored.

Use correlation 0 for independent outcomes

When using a correlation vector, it must match the number of test pairs:
`ntest*(ntest-1)/2`, in this order: (1,2), (1,3), ..., (1,ntest), (2,3),
..., (ntest-1,ntest).

The covariance matrix is derived from the correlation matrix and the
standard deviations.

For example: with `ntest = 3` and `corr = c(0.2, 0.3, 0.4)`, the
resulting correlation matrix is:

|         |        |        |        |
|---------|--------|--------|--------|
|         | \[,1\] | \[,2\] | \[,3\] |
| \[1, \] | 1      | 0.2    | 0.3    |
| \[2, \] | 0.2.   | 1      | 0.4    |
| \[3, \] | 0.3.   | 0.4    | 1      |

## See also

[`empirical_power_result`](https://johnaponte.github.io/ssutil/reference/empirical_power_result.md)

## Examples

``` r
sim_power_ni_normal(
  nsim = 1000,
  npergroup = 250,
  ntest = 7,
  ni_limit = log10(2/3),
  test_req = 2,
  test_opt = 3,
  sd = 0.4,
  corr = 0,
  t_level = 0.05
)
#> Empirical Power Result
#> ----------------------- 
#> Power:       1.0000
#> 95% CI:      [0.9963, 1.0000]
#> Simulations: 1000
```
