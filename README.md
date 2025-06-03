# ssutil

**ssutil** is an R package providing a suite of tools for sample size 
estimation and power simulation in a variety of clinical trial designs. It 
includes methods for binomial, normal, and negative binomial endpoints, as 
well as support for equivalence and non-inferiority testing scenarios. It 
also includes functionality for power and sample size calculation for 
selecting the best group using the indifferent-zone approach for normal and 
binomial outcomes.

## Features

- Empirical power simulation for:
  - Binomial, normal, and negative binomial endpoints
  - Equivalence and non-inferiority designs
  - Best group selection using the indifferent-zone approach for normal and 
    binomial outcomes
- Sample size utilities for various design types

## Installation

```r
# Install from GitHub (requires remotes or devtools)
remotes::install_github("johnaponte/ssutil")
```

## Vignettes

Learn how to use `ssutil` through these worked examples:

- [Selection of the best group using indifferent zone approach for binomial outcomes](https://johnaponte.github.io/ssutil/articles/iz_binomial.html)

- [Selection of the best group using indifferent zone approach for normal outcomes](https://johnaponte.github.io/ssutil/articles/iz_normal.html)

- [Equivalence Trials](https://johnaponte.github.io/ssutil/articles/equivalence.html)

- [Negative Binomial Models](https://johnaponte.github.io/ssutil/articles/negative_binomial.html)

- [Non-Inferiority Testing](https://johnaponte.github.io/ssutil/articles/non_inferiority.html)

- [Single Rate Power](https://johnaponte.github.io/ssutil/articles/power_single_rate.html)


