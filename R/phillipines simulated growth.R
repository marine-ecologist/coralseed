

# Load necessary libraries
library(tidyverse)

# Data from dexter and pH
growth_data_ph <- tibble(
  date = as.Date(c('2013-05-05','2014-02-01', '2014-05-01', '2014-08-01', '2014-10-01', '2014-12-01',
                   '2015-02-01', '2015-04-01', '2015-12-01', '2016-03-01')),
  width = c(0, 2.10, 3.33, 5.33, 7.09, 8.85, 9.34, 10.86, 12.47, 15.31),
  se = c(0, 0.44, 0.34, 0.34, 0.73, 0.78, 0.68, 1.27, 1.56, 1.61)
  )


# Number of simulations
n_simulations <- 100

# Simulate growth for each simulation iteration
philipines_sim <- map_dfr(1:n_simulations, ~{
  growth_data_ph_it <- growth_data_ph %>%
    rowwise() %>%
    mutate(width = rnorm(1, mean = width, sd = se)*10) %>%
    ungroup() %>%
    mutate(sim = .x)

  return(growth_data_ph_it)
}) |>
  mutate(convert_time = date-as.Date("2013-05-05"))
#
# philipines_sim |>
#   group_by(date) |>
#   summarise(width=mean(width)) |>
#   ggplot() + theme_bw() +
#     geom_col(aes(date,width))

philipines_sim_2017 <- philipines_sim |>
  mutate(time=as.Date("2017/01/01") + convert_time) |>
  mutate(time = year(time) + (yday(time) - 1) / (if_else(leap_year(time), 366, 365)))

# Plot the growth trajectories
ggplot() + theme_bw() +
  geom_line(data = philipines_sim_2017, aes(x = time, y = width, group = sim, color = sim), show.legend=FALSE, alpha = 0.3) +
  geom_point(data = philipines_sim_2017, aes(x = time, y = width, group = sim, fill = sim), shape=21, show.legend=FALSE, alpha = 0.3) +
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50)) +
  scale_x_continuous(limits = c(2016, 2023), breaks = seq(2016, 2023, 1)) +
  labs(x = "Year", y = "Width (mm)", title = "Simulated Growth Trajectories Over Time")


library(brms)


prior1 <- prior(normal(1, 4), nlpar = "b1") + prior(normal(0, 2), nlpar = "b2")
fit1 <- brm(width ~ exp(as.numeric(convert_time)), data = philipines_sim_2017)

plot(conditional_effects(fit1), points = TRUE)



# Define priors for the parameters a and b
prior_allometric <- prior(normal(0, 2), nlpar = "a") +
  prior(normal(1, 2), nlpar = "b")

# Formula for the non-linear allometric growth model
allometric_formula <- bf(width ~ a * time^b, a + b ~ 1, nl = TRUE)

# Fit the model
fit_allometric <- brm(allometric_formula, data = philipines_sim_2017, prior = prior_allometric)

# Plot the model
plot(fit_allometric)

conditional_effects(fit_allometric, "time")




# Define a simpler prior with only two parameters
prior2 <- prior(normal(1, 2), nlpar = "a") + prior(normal(0, 2), nlpar = "b")

# Fit a simplified exponential model y = a * exp(b * x)
fit2 <- brm(bf(width ~ a * exp(b * time), a + b ~ 1, nl = TRUE),
            data = philipines_sim_2017, prior = prior2)

conditional_effects(fit2, "time")

philipines_sim_2017 <- philipines_sim_2017 |> mutate(time2 = as.numeric(convert_time))

library(brms)

# Define priors for the parameters K (carrying capacity), r (growth rate), and y0 (initial size)
prior_logistic <- prior(normal(100, 50), nlpar = "K") +     # Prior for carrying capacity
  prior(normal(0.1, 0.05), nlpar = "r") +   # Prior for growth rate
  prior(normal(10, 5), nlpar = "y0")        # Prior for initial size

# Define the formula for the logistic growth model
logistic_formula <- bf(width ~ K / (1 + ((K - y0) / y0) * exp(-r * time2)),
                       K + r + y0 ~ 1, nl = TRUE)

# Fit the model
fit_logistic <- brm(logistic_formula, data = philipines_sim_2017, prior = prior_logistic)

# Plot the model
plot(fit_logistic)

conditional_effects(fit_logistic, "time2")# + ylim(0,100)





# Define the formula for the three-parameter logistic growth model
three_parameter_growth <- bf(
  width ~ asymptote / (1 + exp(-(time2 - midpoint) / scale)),
  asymptote + midpoint + scale ~ 1, # Each parameter is a constant
  nl = TRUE # Non-linear model
)

four_parameter <- bf(
  width ~ asymptote + (0.1-(asymptote)) / (1+((time2/(midpoint))^(scale))),
  asymptote + midpoint + scale ~ 1, # Each parameter is a constant
  nl = TRUE # Non-linear model
)



# Define priors for the parameters
prior_three_four_param <- prior(normal(100, 50), nlpar = "asymptote") +   # Prior for asymptote (carrying capacity)
  prior(normal(100, 50), nlpar = "midpoint") +    # Prior for midpoint (growth midpoint)
  prior(normal(10, 5), nlpar = "scale")           # Prior for scale (growth rate)

# Fit the non-linear model with a Gaussian family
fit_logistic_phillipines <- brm(
  formula = four_parameter,
  data = philipines_sim_2017,
  prior = prior_three_four_param,
  family = gaussian(),
  chains = 4, iter = 2000
)

# Plot the model
plot(fit_logistic_phillipines)
plot(conditional_effects(fit_logistic_phillipines, "time2"), points=TRUE)# + ylim(0,100)
