# ssutil 0.15.0
- Remove lf_config functions as they are not useful
- Modify `sim_best_binomial()` function to select the best using ranks rather
  than the custom approach of which == max().
- Make uniform the parameters names
- Provide standardized output for the simulations

# ssutil 0.14.0
- Re-factoring of the entire package.
- Functions that calculate power based on simulation now start with the prefix
  `sim_`.
- `ni_fleming` has been renamed to `ni_ve`.
- The `multz` function was modified to correctly handle the case when there is
  only one group.
- Improve documentation

# ssutil 0.13.0
- Added the `probhr` function to estimate the probability of an event in the
  experimental group based on the control group probability and the hazard
  ratio, assuming proportional hazards.

# ssutil 0.12.0
- Added the option to preserve the effect.
- Modified the formula used in the Fleming method.

# ssutil 0.11.0
- Added a 30% criterion to the `ni_fleming` function.

# ssutil 0.10.1
- Updated the messages displayed by `ni_fleming`.

# ssutil 0.10.0
- `power_best_binomial` and `power_best_normal` were updated to match their
  rank-based equivalents.
- Now, `dif` represents the difference between the most promising group and
  the rest.
- For binomial tests, `prob` is now the probability in the most promising
  group (previously it referred to the other groups), and `dif` defines how
  much lower the other groups are.

# ssutil 0.9.0
- Added support for weights in `power_best_norm_ranks` and `power_best_bin_rank`.
- Note: the API is not backward compatible for these two functions.

# ssutil 0.8.0
- Added `power_best_norm_rank` to empirically estimate power for normal
  distributions based on ranks.

# ssutil 0.7.0
- Added a function to calculate non-inferiority in vaccine trials following
  Fleming et al.

# ssutil 0.6.0
- Fixed an error in `power_test_binomial` where, in the absence of ties,
  the `sample` function randomly selected a number instead of sampling from
  a range.
- Fixed the same issue in `power_best_normal`.
- `power_best_binomial` now returns a data frame with power and 95%
  confidence interval.
- `power_best_normal` now returns a data frame with power and 95%
  confidence interval.
- `power_ni_normal` now includes the number of simulations in the output data
  frame.
- Added `power_best_bin_rank`, which selects the best group based on ranks,
  instead of assuming the highest value as in `power_best_binomial`.

# ssutil 0.5.0
- Added the `power_ni_normal` function.

# ssutil 0.4.1
- Fixed an error in the `ggplot_prob_lowest_power` graph.
- Fixed some global variables and cleaned up imports.

# ssutil 0.4.0
- Added the `power_equivalence_normal` function.

# ssutil 0.3.0
- Added the `power_best_binomial` function and related helpers.

# ssutil 0.2.0
- Added the `power_best_normal` function and related helpers.

# ssutil 0.1.1
- Added a `NEWS.md` file to track package changes.
- Added the `power_single_rate` function.
