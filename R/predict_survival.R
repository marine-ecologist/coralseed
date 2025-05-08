# For a lognormal distribution in the context of time-to-event data, the survival function
# S(t) does not have a simple exponential form as seen with the exponential or Weibull distributions.
# The lognormal survival function can be expressed in terms of the standard normal cumulative distribution function (CDF),
# Φ, because the log of the time-to-event follows a normal distribution.

# Given a lognormal distribution with parameters
# μ (mean on the log scale) and
# σ (standard deviation on the log scale), the survival function at time
# t is given by:
#
# S(t)=1−Φ(
#   σ
#   log(t)−μ
# )

# In R, you can use the pnorm function to calculate the standard normal CDF,
# Φ. Here is how you can mutate your dataframe to include the survival probability for a lognormal distribution:

predict_survival <- function(input){

dataset <- map(1:10000, function(i) {
  parameter_draws_sample <- input %>% slice_sample(n = 1)
  newdat <- data.frame(minutes = seq(1, 620, 1)) %>%
    mutate(settlement_probability = 1 - pnorm((log(minutes) -parameter_draws_sample$b_Intercept) / parameter_draws_sample$sigma)) %>%
    mutate(id = i)

  return(newdat)
})

dataset_grouped <- do.call(rbind, dataset) %>%
  mutate(outcome = rbinom(nrow(.), 1, settlement_probability))

dataset_grouped_subset <- dataset_grouped %>% group_by(id) %>% slice_sample(n=100)
dataset_grouped_median <- dataset_grouped %>% group_by(minutes) %>% summarise(settlement_probability=median(settlement_probability))

# Plot
ggplot() + theme_bw() + xlab("Hours after release") + ylab("Settlement Probability") + ylim(0,1) +
  geom_line(data=dataset_grouped_subset, aes(x=minutes/60, y=settlement_probability, group=id), linewidth=0.05, color="slategrey") +
  geom_line(data=dataset_grouped_median, aes(x=minutes/60, y=settlement_probability), linewidth=1, color="black")


}
