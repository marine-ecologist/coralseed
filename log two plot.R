library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)

set.seed(NULL)
n_simulations <- 100

# Adapt to lognormal model
simulated_data <- map_dfr(1:n_simulations, function(i) {
  draw <- parameter_draws_log |> slice_sample(n=1)

  b_Intercept <- draw$b_Intercept
  sigma <- draw$sigma
  mu <- b_Intercept  # In log-normal, mu is the mean of the log of the variable

  # Calculate the time to competency based on the lognormal model
  minutes <- exp(mu + sigma * rnorm(1))

  # Calculate the competency probability at the time to competency
  competency_probability <- 1 - plnorm(minutes, meanlog = mu, sdlog = sigma)

  data.frame(id = i,
             minutes = minutes,
             sigma = sigma,
             mu = mu,
             b_Intercept = b_Intercept,
             competency_probability = competency_probability)
})

# Time range for the lognormal survival curve

# Calculate survival probabilities distribution (curves) for each id
predicted_curves <- simulated_data %>%
  group_by(id) %>%
  do(data.frame(minutes = minutes,
                competency_probability = 1 - plnorm(minutes, meanlog = .$mu, sdlog = .$sigma)))

predicted_median <- predicted_curves %>%
  group_by(minutes) %>%
  summarise(competency_probability = median(competency_probability))

predicted_point <- simulated_data |> dplyr::filter(minutes <= max(minutes))

# Histogram of simulated times
p1 <- ggplot() + theme_bw() + xlim(0,48) +
  geom_histogram(data = simulated_data, aes(x = minutes/60), bins = 30, fill = "turquoise4", color = "black", linewidth = 0.25, alpha = 0.3) +
  geom_vline(xintercept = 12) +
  geom_density(data = simulated_data, aes(x = minutes/60, y = ..count..), color = "coral", size = 1, alpha = 1) +  # Add KDE with the same y-scale
  labs(title = paste0("Simulated Time to Competency (Lognormal Model) = ",
                      length(unique(pull(simulated_data |> dplyr::filter(minutes < 12*60), id))),
                      "/", length(unique(simulated_data$id)), " settled by ", round(max(minutes)/60), " hours"),
       x = "",
       y = "Frequency")

# Predicted survival curves and points
p2 <- ggplot() + theme_bw() + xlim(0,48) +
  geom_line(data = predicted_curves, aes(x = minutes/60, y = competency_probability, group = id), color = "turquoise4", alpha = 0.1) +
  geom_line(data = predicted_median, aes(x = minutes/60, y = competency_probability), color = "black", alpha = 1.5) +
  geom_vline(xintercept = 12, alpha = 0.5) +
  geom_point(data = simulated_data, aes(x = minutes/60, y = competency_probability), color = "black", fill = "coral", shape = 21, stroke = 0.3, size = 2, alpha = 1) +
  labs(title = "Predicted Lognormal Survival Curves",
       x = "Time to Competency (hours)",
       y = "Probability")

# Display the plots
p1 / p2
