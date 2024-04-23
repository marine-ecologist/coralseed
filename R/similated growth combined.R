

# Load necessary libraries
library(tidyverse)
library(ggplot2)


### Phillipines

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
all_simulations <- map_dfr(1:n_simulations, ~{
  growth_data_ph_it <- growth_data_ph %>%
    rowwise() %>%
    mutate(width = rnorm(1, mean = width, sd = se)*10) %>%
    ungroup() %>%
    mutate(sim = .x)

  return(growth_data_ph_it)
}) |>
  mutate(convert_time = date-as.Date("2013-05-05"))
#
# all_simulations |>
#   group_by(date) |>
#   summarise(width=mean(width)) |>
#   ggplot() + theme_bw() +
#     geom_col(aes(date,width))

all_simulations_2017 <- all_simulations |>
  mutate(time=as.Date("2017/01/01") + convert_time) |>
  mutate(time = year(time) + (yday(time) - 1) / (if_else(leap_year(time), 366, 365)))




### Heron Island

# Load necessary libraries
library(tidyverse)

# Data from Doropoulos et al PLoS ONE
growth_data <- tibble(
  size_bin = c("0-10", "11-20", "21-30", "31-40", "41-50", ">50"),
  mean_growth = c(5.66, 8.58, 9.09, 11.76, 12.9, 16.65),
  SE = c(1.59, 1.65, 2.10, 1.59, 1.97, 1.91)
)

# Function to simulate growth and return a dataframe with time, width, and sim number

# The function simulate_growth initializes a data frame to track growth over time (time),
# the current size (width), and the simulation number (sim). It starts each simulation
# with a size of 0mm. For each timestep, the function determines the appropriate size bin
# based on the current size, samples growth from a normal distribution centered on the mean
# growth for that bin with variability introduced by the standard error, and updates the size.



# Function to simulate growth and return a dataframe with time, width, and sim number
simulate_growth <- function(sim_number, n_steps = 10) {
  # Initialize starting point
  growth_history <- tibble(
    time = 0:n_steps/2,
    width = numeric(n_steps + 1),
    sim = rep(sim_number, n_steps + 1),
    sampled_growth = numeric(n_steps + 1)  # Initialize sampled_growth
  )
  growth_history$width[1] <- 0  # Set initial width to 0mm
  growth_history$sampled_growth[1] <- 0  # Initial growth is 0

  current_size <- 0

  for (i in 1:n_steps) {
    # Determine size bin
    size_bin <- case_when(
      current_size <= 10 ~ 1,
      current_size <= 20 ~ 2,
      current_size <= 30 ~ 3,
      current_size <= 40 ~ 4,
      current_size <= 50 ~ 5,
      TRUE ~ 6
    )

    # Sample growth from the normal distribution
    sampled_growth <- rnorm(1, mean = growth_data$mean_growth[size_bin], sd = growth_data$SE[size_bin])

    # Update size and store the growth for this step
    current_size <- current_size + sampled_growth
    growth_history$width[i + 1] <- current_size  # Store growth at next time point
    growth_history$sampled_growth[i + 1] <- sampled_growth  # Store sampled growth
  }

  return(growth_history)
}

# Number of simulations
n_simulations <- 100

# Run the simulation for each iteration and bind the results
set.seed(NULL)  # for reproducibility
all_growth_results <- bind_rows(lapply(1:n_simulations, simulate_growth, n_steps = 10)) %>%
  mutate(time = 2017 + time)  # Adjust time based on your required starting year


### plot

library(patchwork)

# Plot the growth trajectories
a <- ggplot() +
  geom_line(data = all_simulations_2017, aes(x = time, y = width, group = sim, color = sim), show.legend=FALSE, alpha = 0.3, color="coral3") +
  geom_line(data = all_growth_results, aes(x = time, y = width, group = sim, color = sim), show.legend=FALSE, alpha = 0.3, color="skyblue") +
  theme_bw() +
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50)) +
  scale_x_continuous(limits = c(2016, 2023), breaks = seq(2016, 2023, 1)) +
  labs(x = "Year", y = "Width (mm)", title = "Simulated Growth Trajectories Over Time")


b <-  ggplot() +
  geom_line(data = all_simulations_2017, aes(x = time, y = width, group = sim, color = sim), show.legend=FALSE, alpha = 0.3, color="coral3") +
  geom_line(data = all_growth_results, aes(x = time, y = width, group = sim, color = sim), show.legend=FALSE, alpha = 0.3, color="skyblue") +
  theme_bw() +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  scale_x_continuous(limits = c(2016, 2019), breaks = seq(2016, 2019, 1)) +
  labs(x = "Year", y = "Width (mm)", title = "Simulated Growth Trajectories Over Time")

a + b


library(brms)

combined <- rbind(
  all_growth_results |> select(time, width, sim) |> mutate(location="Heron"),
  all_simulations_2017 |> select(time, width, sim) |> mutate(location="Phillipines")
  )


# Define the non-linear formula
nlform <- bf(
  width ~ exp(b0 + b1 * time + b2 * location),
  b0 + b1 + b2 ~ 1,
  nl = TRUE
)

# Fit the model
model <- brm(
  formula = nlform,
  data = combined,
  family = gaussian(), # Assuming Gaussian noise around the exponential curve
  chains = 2,           # Number of Markov Chains
  cores = 2,            # Number of cores used for parallel processing
  iter = 2000           # Number of iterations per chain
)




library(tidybayes)
library(modelr)


# Assuming you have a model fitted with 'mu ~ exp(b0 + b1 * time + b2 * location)'
posterior_b0 <- posterior_samples(model, variables = "b0")
posterior_b1 <- posterior_samples(model, variables = "b1")
posterior_b2 <- posterior_samples(model, variables = "b2")

# Plotting distributions of b0, b1, b2
library(ggplot2)
ggplot(posterior_b0, aes(x = b0)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Posterior Distribution of b0 (Baseline Effect)", x = "b0", y = "Density")

ggplot(posterior_b1, aes(x = b1)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Posterior Distribution of b1 (Rate of Increase with Time)", x = "b1", y = "Density")

ggplot(posterior_b2, aes(x = b2)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Posterior Distribution of b2 (Modification by Location)", x = "b2", y = "Density")
