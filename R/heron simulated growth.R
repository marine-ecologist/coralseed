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

# Plot the growth trajectories
ggplot() +
  geom_line(data = all_growth_results, aes(x = time, y = width, group = sim, color = sim), show.legend=FALSE, alpha = 0.3) +
  theme_bw() +
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50)) +
  scale_x_continuous(limits = c(2016, 2024), breaks = seq(2016, 2024, 1)) +
  labs(x = "Year", y = "Width (mm)", title = "Simulated Growth Trajectories Over Time")
