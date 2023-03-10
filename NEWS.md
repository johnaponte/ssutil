# ssutil 0.9.0
-   Inclusion of weights on power_best_norm_ranks and power_best_bin_rank to allow.
    The API is not backwards compatible for this two functions.

# ssutil 0.8.0
-   power_best_norm_rank: to empirically find the power for normal distributions
    based on ranks 
# ssutil 0.7.0
-   Add a function to calculate non-inferiority in vaccine trials following
    Flemming et al
# ssutil 0.6.0
-   Fixed an error on power_test_binomial as when not a tie, the sample 
    function select randomly a number instead between 1 and the number, 
    and not a sample of the number!!! Odd behavior of the sample function.
    
-   Fixed the same problem in power_best_normal    
    
-   power_best_binomial return a dataframe with the power and 95% CI    

-   power_best_normal returns a dataframe with the power and 95% CI

-   power_ni_normal add number of simulations to the output dataframe
    
-   Add power_best_bin_rank, that select the best but based on ranks, instead
    of being always the best as in power_best binomial.
# ssutil 0.5.0

-   Added the power_ni_normal function

# ssutil 0.4.1

-   Fix a mistake in the graphs ggplot_prob_lowest_power
-   Fix some global variables and imports

# ssutil 0.4.0

-   Added the function power_equivalence_normal

# ssutil 0.3.0

-   Added the function power_best_binomial and associates

# ssutil 0.2.0

-   Added the function power_best_normal and associates

# ssutil 0.1.1

-   Added a `NEWS.md` file to track changes to the package.
-   Added the function power_single_rate
