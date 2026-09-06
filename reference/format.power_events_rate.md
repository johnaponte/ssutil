# Format method for power_events_rate class

Format method for power_events_rate class

## Usage

``` r
# S3 method for class 'power_events_rate'
format(x, digits = 1, ...)
```

## Arguments

- x:

  an R object of class power_events_rate

- digits:

  a positive integer indicating how many decimal digits are to be used
  to display the probability columns as percentages.

- ...:

  further arguments passed to or from other methods

## Value

A character string with a markdown-style table of the probabilities of
observing each event threshold, by sample size and risk.
