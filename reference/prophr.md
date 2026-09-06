# Calculate Event Probability in the Experimental Group Given a Hazard Ratio

Computes the event probability in the experimental group based on the
event probability in the control group and a specified hazard ratio,
assuming proportional hazards.

## Usage

``` r
prophr(p0, hr)
```

## Arguments

- p0:

  Numeric scalar. Probability of an event in the control group (between
  0 and 1).

- hr:

  Numeric scalar. Hazard ratio (must be \> 0).

## Value

Numeric. The probability of an event in the experimental group.

## Details

This is useful for sample size calculations, for example in PASS (TM),
which does not automatically adjust the event rate for the experimental
group.

## Examples

``` r
prophr(0.05, 0.6)
#> [1] 0.03030722
```
