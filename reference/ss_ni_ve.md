# Sample Size and Non-Inferiority Margin for Vaccine Efficacy Trials

Computes the non-inferiority margin, number of events, and maximum
hazard ratio (HR) to declare non-inferiority in vaccine efficacy (VE)
trials, based on the approach described by Fleming et al. (2021).

## Usage

``` r
ss_ni_ve(
  ve_lci,
  alpha = 0.025,
  power = 0.9,
  use70 = FALSE,
  preserve = 0.5,
  ve_exp = NULL,
  ve_ac = NULL
)
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

- ve_exp:

  Numeric or `NULL` (default). Assumed vaccine efficacy of the
  experimental vaccine versus placebo, used to compute power for the
  non-inferiority trial. If `NULL` (the default), the experimental
  vaccine is assumed to have the same true efficacy as the active
  comparator (hazard ratio of 1 between them). If specified, `ve_ac`
  must also be provided.

- ve_ac:

  Numeric or `NULL` (default). Point estimate of the active comparator's
  vaccine efficacy versus placebo (as opposed to `ve_lci`, which is its
  confidence interval lower bound). Only used, and required, when
  `ve_exp` is specified.

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

This implementation approximates Tables 1 and 2 of the paper: the total
number of events uses Freedman's (1982) log-rank sample size formula,
generalized to allow the true experimental-to-comparator hazard ratio
assumed for power (`ve_exp`) to differ from 1; the maximum hazard ratio
to declare non-inferiority uses an exact binomial confidence interval
via `binom.test`.

## References

Fleming, T.R., Powers, J.H., & Huang, Y. (2021). The use of active
controls and non-inferiority studies in evaluating COVID-19 vaccines.
*Clinical Trials*, 18(3), 335–342.
[doi:10.1177/1740774520988244](https://doi.org/10.1177/1740774520988244)

Freedman, L.S. (1982). Tables of the number of patients required in
clinical trials using the logrank test. *Statistics in Medicine*, 1(2),
121-129.
[doi:10.1002/sim.4780010204](https://doi.org/10.1002/sim.4780010204)

## Examples

``` r
# Table 1: rule out margin delta, assuming EXP has the same true efficacy
# as AC (175-event active-comparator trial, 95% VE, HR upper 95% CI = 0.0855)
ss_ni_ve(ve_lci = 1 - 0.0855, power = 0.9)
#> $`Upper limit of the HR used to estimate the sample size`
#> [1] 0.0855
#> 
#> $`Non-inferior margin in HR scale`
#> [1] 3.419928
#> 
#> $Alpha
#> [1] 0.025
#> 
#> $Power
#> [1] 0.9
#> 
#> $`Total number of events`
#> [1] 36
#> 
#> $`Max HR to declare NI`
#> [1] 1.571429
#> 
#> $`Max number of events in the experimental group`
#> [1] 22
#> 
#> $`Non-inferior criteria`
#> [1] "Preserve at least 50% of the VE in the new vaccine"
#> 

# The 90-70 rule (use70 = TRUE) rules out the more lenient margin delta0
# instead of delta, for the same active-comparator trial as above
ss_ni_ve(ve_lci = 1 - 0.0855, power = 0.9, use70 = TRUE)
#> $`Upper limit of the HR used to estimate the sample size`
#> [1] 0.0855
#> 
#> $`Non-inferior margin in HR scale`
#> [1] 9.785497
#> 
#> $Alpha
#> [1] 0.025
#> 
#> $Power
#> [1] 0.9
#> 
#> $`Total number of events`
#> [1] 16
#> 
#> $`Max HR to declare NI`
#> [1] 2.2
#> 
#> $`Max number of events in the experimental group`
#> [1] 11
#> 
#> $`Non-inferior criteria`
#> [1] "At least 30% VE in new vaccine"
#> 

# Table 2: rule out margin delta0, assuming the experimental vaccine has a
# fixed 60% efficacy versus placebo regardless of AC's own efficacy (here,
# AC has 60% VE, HR upper 95% CI = 0.4997)
ss_ni_ve(ve_lci = 1 - 0.4997, power = 0.9, use70 = TRUE, ve_exp = 0.60, ve_ac = 0.60)
#> $`Upper limit of the HR used to estimate the sample size`
#> [1] 0.4997
#> 
#> $`Non-inferior margin in HR scale`
#> [1] 1.674325
#> 
#> $Alpha
#> [1] 0.025
#> 
#> $Power
#> [1] 0.9
#> 
#> $`Total number of events`
#> [1] 166
#> 
#> $`Max HR to declare NI`
#> [1] 1.213333
#> 
#> $`Max number of events in the experimental group`
#> [1] 91
#> 
#> $`Non-inferior criteria`
#> [1] "At least 30% VE in new vaccine"
#> 
```
