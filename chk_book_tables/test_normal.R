# This test power_normal
library(tidyverse)
library(furrr)

plan(multisession, workers = 8)
sim <-
  expand.grid(
    dif = runif(6, 0.5, 1.5),
    sd = runif(6, 0.8, 2),
    ngroups = sample(2:6, 6, replace = T),
    npergroup = sample(5:15, 6, replace = T)
  ) |>
  mutate(id = row_number()) |>
  nest(data = -id) |>
  mutate(c = future_map(
    data, 
    ~ power_best_normal(.$dif, .$sd, .$ngroups, .$npergroup),
    .options = furrr_options(seed = 123)
  )) |>
  mutate(b = future_map(
    data,
    ~ sim_power_best_normal(
      noutcomes = 1,
      dif = .$dif,
      sd = .$sd,
      ngroups = .$ngroups,
      npergroup = .$npergroup,
      nsim = 1000
    ),
    .options = furrr_options(seed = 123)
  )) |>
  mutate(r = future_map(
    data,
    ~ sim_power_best_norm_rank(
      noutcomes = 1,
      dif = .$dif,
      sd = .$sd,
      weights = 1,
      ngroups = .$ngroups,
      npergroup = .$npergroup,
      nsim = 1000
    ),
    .options = furrr_options(seed = 123)
  )) |>
  unnest(c(c, b, r), names_sep = "_") |>
  mutate(`Sim best` = c - b_power) |>
  mutate(`Sim rank` = c - r_power) |>
  select(id, `Sim best`, `Sim rank`) |>
  pivot_longer(-id, names_to = "Procedure", values_to = "diference")



sim |> 
  ggplot() + 
  aes(x = diference, color = Procedure, fill = Procedure) + 
  geom_density(alpha = 0.3) + 
  ggtitle("Difference between analytical and simulated power")

# It is expected and average difference of 0 showing no bias,
# and similar curves by Best and Rank options as for 1 outcome
# it makes no difference.
