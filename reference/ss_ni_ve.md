# Sample Size and Non-Inferiority Margin for Vaccine Efficacy Trials

Computes the non-inferiority margin, number of events, and maximum
hazard ratio (HR) to declare non-inferiority in vaccine efficacy (VE)
trials, based on the approach described by Fleming et al. (2021).

## Usage

``` r
ss_ni_ve(ve_lci, alpha = 0.025, power = 0.9, use70 = FALSE, preserve = 0.5)
```

## Arguments

- ve_lci:

  Numeric. Lower bound of the current vaccine's efficacy (e.g., 0.95 for
  95% VE).

- alpha:

  Numeric. Type I error rate (default = 0.025).

- power:

  Numeric. Desired power for the test (default = 0.90).

- use70:

  Logical. If `TRUE`, assumes at least 30% VE for the new vaccine (the
  90–70 rule); otherwise, preserves a fixed fraction of the reference
  VE.

- preserve:

  Numeric. Proportion of the current vaccine's efficacy to preserve
  under `use70 = FALSE` (default = 0.5).

## Value

A named list with:

- Upper limit of the HR used to estimate the sample size: Hazard ratio
  corresponding to `ve_lci`.

- Non-inferior margin in HR scale: Non-inferiority margin expressed as a
  hazard ratio.

- Alpha: The type I error used.

- Power: The power used.

- Total number of events: Total number of events required in the trial.

- Max HR to declare NI: Maximum observed hazard ratio that satisfies the
  non-inferiority criterion.

- Max number of events in the experimental group: Maximum number of
  events in the experimental group still compatible with
  non-inferiority.

- Non-inferior criteria: Description of the applied non-inferiority rule
  ("At least 30% VE" or "or preserved effect").

## Details

The method applies either the 95–95 rule or 90–70 rule, depending on
whether a minimum VE of 30% is assumed (`use70 = TRUE`) or 50% of the
current VE is preserved.

This implementation approximates Table 1 of the paper using exact
binomial confidence intervals via `binom.test` and the
`nBinomial1Sample` function from gsDesign.

## References

Fleming, T.R., Powers, J.H., & Huang, Y. (2021). The use of active
controls and non-inferiority studies in evaluating COVID-19 vaccines.
*Clinical Trials*, 18(3), 335–342.
[doi:10.1177/1740774520988244](https://doi.org/10.1177/1740774520988244)

## Examples

``` r
ss_ni_ve(ve_lci = 0.95)
#> $`Upper limit of the HR used to estimate the sample size`
#> [1] 0.05
#> 
#> $`Non-inferior margin in HR scale`
#> [1] 4.472136
#> 
#> $Alpha
#> [1] 0.025
#> 
#> $Power
#> [1] 0.9
#> 
#> $`Total number of events`
#> [1] 28
#> 
#> $`Max HR to declare NI`
#> [1] 1.8
#> 
#> $`Max number of events in the experimental group`
#> [1] 18
#> 
#> $`Non-inferior criteria`
#> [1] "Preserve at least 50% of the VE in the new vaccine"
#> 
```
