# Power for rare events

## Introduction

One important question in drug safety monitoring or rare event studies
is:

> “**Given a specific sample size and the desired statistical power,
> what is the smallest event rate (proportion) that can be reliably
> detected (i.e., at least one event expected with a set
> probability)?**”

[`power_single_rate()`](https://johnaponte.github.io/ssutil/reference/power_single_rate.md)
answers this for a single event: the minimum true event rate needed for
a given sample size and power to observe at least one event.
[`power_events_rate()`](https://johnaponte.github.io/ssutil/reference/power_events_rate.md)
generalizes the question the other way around: **given a sample size and
an assumed event rate, what is the probability of observing at least 1,
2, 3, … events?**

Both are useful in clinical trial planning and post-marketing safety
surveillance, where the event of interest (such as a serious adverse
reaction) is rare, but assuring a high probability of observing it if
the true rate is high enough is crucial for safety oversight.

## `power_single_rate()`

The function signature is:

``` r

power_single_rate(subjects, power)
```

- `subjects`: Integer or vector. Sample size(s).
- `power`: Numeric or vector. Desired power(s), between 0 and 1.

The function returns a matrix (class `power_single_rate`) with
columns: - `n`: sample size, - `power`: statistical power, -
`proportion`: minimum detectable event rate.

A formatted print method is provided for readable output.

### Example 1: 100 Subjects, Power 0.95

Suppose you want to know the lowest event rate that would provide a 95%
chance of observing at least one event among 100 subjects.

``` r

library(ssutil)
power_single_rate(100, 0.95)
```

    ## A study with 100 participants would have 95% power to detect at least one event
    ## if the true event rate is at least 2.95 per 100 participants.

### Example 2: 30 Subjects, Power 0.95, 0.90, and 0.8

Suppose your sample size is 30, and you want to know what true event
rate you can potentially detect (at least one event) with powers of 95%,
90%, and 80%.

``` r

power_single_rate(30, c(0.95, 0.90, 0.8))
```

    ## According to the number of participants, the table shows the power
    ## to detect at least one event, given a true event rate equal to or higher than:
    ## 
    ## | Subjects | Power |                Proportion |
    ## | -------- | ----- | ------------------------- |
    ## |       30 |   95% | 9.50 per 100 participants |
    ## |       30 |   90% | 7.39 per 100 participants |
    ## |       30 |   80% | 5.22 per 100 participants |

### Example 3: 500 and 1500 Subjects

Larger safety databases are common more advanced phases of clinical
development. With 500 and 1500 subjects, the minimum detectable event
rate for 95% power is:

``` r

power_single_rate(c(500, 1500), 0.95)
```

    ## According to the number of participants, the table shows the power
    ## to detect at least one event, given a true event rate equal to or higher than:
    ## 
    ## | Subjects | Power |                 Proportion |
    ## | -------- | ----- | -------------------------- |
    ## |      500 |   95% | 5.97 per 1000 participants |
    ## |     1500 |   95% | 2.02 per 1000 participants |

## `power_events_rate()`

[`power_single_rate()`](https://johnaponte.github.io/ssutil/reference/power_single_rate.md)
answers the “at least one event” case, and solves for the rate. As a
complement, it is useful to go the other way: assume a known or
suspected event rate, and ask for the probability of observing at least
1, 2, or more events. A natural set of rates to check against is the
standard adverse-event frequency categories used in drug and vaccine
labeling (EMA, 2009; CIOMS Working Groups III and V, 1999):

| Category          | Frequency                |
|-------------------|--------------------------|
| Very common       | ≥ 1/10                   |
| Common (frequent) | ≥ 1/100 to \< 1/10       |
| Uncommon          | ≥ 1/1,000 to \< 1/100    |
| Rare              | ≥ 1/10,000 to \< 1/1,000 |
| Very rare         | \< 1/10,000              |

[`power_events_rate()`](https://johnaponte.github.io/ssutil/reference/power_events_rate.md)
computes the exact binomial probability of observing at least `e`
events, for every combination of sample size (`n`) and risk (`r`):

``` r

power_events_rate(n, r, e)
```

- `n`: Integer or vector. Sample size(s).
- `r`: Numeric or vector. Risk(s) (per-subject event probability),
  between 0 and 1.
- `e`: Integer or vector. Event count threshold(s), e.g. 1, 2, 3.

The function returns a matrix (class `power_events_rate`) with columns
`N`, `Risk`, and one column per threshold in `e`.

### Example 4: 500 and 1500 Subjects Across Standard Frequency Categories

Using the boundary rate of each category above (1/10, 1/100, 1/1,000,
1/10,000, and 1/100,000 to represent “very rare”), the probability of
observing at least 1, 2, or 3 events with 500 or 1500 subjects is:

``` r

power_events_rate(c(500, 1500), c(1 / 10, 1 / 100, 1 / 1000, 1 / 10000, 1 / 1e5), c(1, 2, 3))
```

    ## |    N |     Risk |     ≥1 |     ≥2 |     ≥3 |
    ## | ---- | -------- | ------ | ------ | ------ |
    ## |  500 |     1/10 | 100.0% | 100.0% | 100.0% |
    ## |  500 |    1/100 |  99.3% |  96.0% |  87.7% |
    ## |  500 |   1/1000 |  39.4% |   9.0% |   1.4% |
    ## |  500 |  1/10000 |   4.9% |   0.1% |   0.0% |
    ## |  500 | 1/100000 |   0.5% |   0.0% |   0.0% |
    ## | 1500 |     1/10 | 100.0% | 100.0% | 100.0% |
    ## | 1500 |    1/100 | 100.0% | 100.0% | 100.0% |
    ## | 1500 |   1/1000 |  77.7% |  44.2% |  19.1% |
    ## | 1500 |  1/10000 |  13.9% |   1.0% |   0.1% |
    ## | 1500 | 1/100000 |   1.5% |   0.0% |   0.0% |

As the table shows, a safety database of 500 or 1500 subjects has a high
probability of detecting at least one “common” or “very common” adverse
event, but very limited power to detect “rare” or “very rare” events -
illustrating why larger post-marketing exposure is needed to
characterize infrequent risks.

## References

European Medicines Agency. (2009). Section 4.8: Undesirable effects.
<https://www.ema.europa.eu/en/documents/presentation/presentation-section-48-undesirable-effects_en.pdf>

CIOMS Working Groups III and V. (1999). Guidelines for Preparing Core
Clinical-Safety Information on Drugs (2nd ed.). Council for
International Organizations of Medical Sciences. ISBN 978-92-9036-070-4.
<https://cioms.ch/wp-content/uploads/2018/03/Guidelines-for-Preparing-Core-Clinical-Safety-Info-Drugs-Report-of-CIOMS-Working-Group-III-and-V.pdf>
