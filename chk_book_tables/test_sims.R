# Test the simulations and the exact formula

library(tidyverse)
library(purrr)

prob = seq(0.05, 0.95, by = 0.01)
dif = 0.1
ngroups = 3
npergroup = 50
nsim = 1000


sim <- expand.grid(
  p1 = prob,
  dif = dif,
  ngroups = ngroups,
  npergroup = npergroup,
  nsim = nsim
) |>
  filter(p1 - dif > 0 & p1 - dif < 1) |>
  mutate(idsim = row_number()) |>
  nest(data = -idsim) |>
  mutate(powersim = map(
    data,
    ~ sim_power_best_binomial(
      noutcomes = 1,
      p1 = .$p1,
      dif = .$dif,
      ngroups = .$ngroups,
      npergroup = .$npergroup,
      nsim = .$nsim
    ) |> tidy() |> select(-nsim)
  )) |>
  mutate(powerexact = map(
    data,
    ~ power_best_binomial(
      p1= .$p1, 
      dif = .$dif, 
      ngroups = .$ngroups, 
      npergroup =.$npergroup))) |>
unnest(c(data, powersim, powerexact))


ggplot(sim) +
  aes(x = p1, y = power) + 
  geom_point() +
  geom_line(aes(y=powerexact))

wcs_power_best_binomial(0.1, 3, 40)
