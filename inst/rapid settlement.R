# https://cran.r-project.org/web/packages/brms/vignettes/brms_families.html#time-to-event-models
# https://bookdown.org/content/ef0b28f7-8bdf-4ba7-ae2c-bc2b1f012283/describing-continuous-time-event-occurrence-data.html


library(tidyverse)
library(ggplot2)

df <- data.frame(
  tile = c('16E', '16E', '16E', '16E', '16E', '16E', '16E', '16D', '16D', '16D', '16D', '16D', '16D', '16D', '16C', '16C', '16C', '16C', '16C', '16C', '16C', '16B', '16B', '16B', '16B', '16B', '16B', '16A', '16A', '16A', '16A', '16A', '16A'),
  minutes = c(620, 360, 300, 255, 180, 120, 60, 620, 360, 300, 255, 180, 120, 60, 620, 360, 300, 255, 180, 120, 60, 620, 300, 255, 180, 120, 60, 620, 300, 255, 180, 120, 60),
  count = c(22, 20, 16, 15, 12, 6, 4, 25, 23, 23, 22, 19, 16, 7, 28, 23, 21, 21, 17, 14, 6, 21, 12, 12, 9, 6, 2, 8, 7, 6, 6, 4, 2),
  density = c(0.88, 0.80, 0.64, 0.60, 0.48, 0.24, 0.16, 1.00, 0.92, 0.92, 0.88, 0.76, 0.64, 0.28, 1.12, 0.92, 0.84, 0.84, 0.68, 0.56, 0.24, 0.84, 0.48, 0.48, 0.36, 0.24, 0.08, 0.32, 0.28, 0.24, 0.24, 0.16, 0.08)
) %>%
  mutate(tile=as.factor(tile)) %>%
  mutate(density=count/25) # divide the cumulative count by area of tile (5x5cm2).

df <- df |> mutate(count2=count*2, count3=count*3)

ggplot() + theme_bw() + ylim(0,100) +
  geom_point(data=df, aes(minutes, 100-count3, color=tile))

library(brms)
library(tidybayes)
library(modelr)
library(tidyverse)


# convert cumulative sum of settled larvae to count data per individual (0 for not settled, 1 for settled) at each time point
df_long1 <- rbind(
  df %>% select(-density) %>%
    uncount(count, .remove = FALSE) %>%
    mutate(settled = 1) %>% select(-count),
  df %>% select(-density) %>%
    uncount(100 - count, .remove = FALSE) %>%
    mutate(settled = 0) %>% select(-count)
)

df_long2 <- rbind(
  df %>% select(-density) %>%
    uncount(count2, .remove = FALSE) %>%
    mutate(settled = 1) %>% select(-count2),
  df %>% select(-density) %>%
    uncount(100 - count2, .remove = FALSE) %>%
    mutate(settled = 0) %>% select(-count2)
)

df_long3 <- rbind(
  df %>% select(-density) %>%
    uncount(count3, .remove = FALSE) %>%
    mutate(settled = 1) %>% select(-count3),
  df %>% select(-density) %>%
    uncount(100 - count3, .remove = FALSE) %>%
    mutate(settled = 0) %>% select(-count3)
)



event_model_exp1 <- brm(minutes | cens(1 - settled) ~ (1 | w | tile),
                       family =  exponential(link = "log"), init = 0,
                       control = list(adapt_delta = 0.99, max_treedepth = 20),
                       cores=11, chains=4, iter = 10000, data = df_long1)

event_model_exp2 <- brm(minutes | cens(1 - settled) ~ (1 | w | tile),
                        family =  exponential(link = "log"), init = 0,
                        control = list(adapt_delta = 0.99, max_treedepth = 20),
                        cores=11, chains=4, iter = 10000, data = df_long2)

event_model_exp3 <- brm(minutes | cens(1 - settled) ~ (1 | w | tile),
                        family =  exponential(link = "log"), init = 0,
                        control = list(adapt_delta = 0.99, max_treedepth = 20),
                        cores=11, chains=4, iter = 10000, data = df_long3)

library(tidybayes)

model_fit1 <- df_long3 %>%
    add_predicted_draws(event_model_exp3, draws=100,  e_formula=NULL) %>%  # adding the posterior distribution
    ggplot(aes(x = minutes, y = count)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                    alpha = 0.5, colour = "black")

    scale_fill_brewer(palette = "Greys") +
    ylab("Calidris canutus abundance\n") +  # latin name for red knot
    xlab("\nYear") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85)))

# Simulate new data for prediction for 1,000 individuals
new_data <- data.frame(id = 1:100000, tile=NA)

# Predict time-to-event for new individuals
event_model_exp1_preds_full <- predict(event_model_log_1, newdata = new_data, ndraws=1000, cores=11, re_formula=NULL, type = "response") |> as.data.frame()
event_model_exp1_preds <- event_model_exp1_preds_full[,1:100] |>
  pivot_longer(everything(), names_to="iters", values_to="competency") |>
  arrange(iters, competency) |>
  mutate(id=rep(rev(1:1000),100)) |>
  mutate(iters = as.numeric(gsub("V", "", iters)))

ggplot() + theme_bw() +
  geom_point(data=event_model_exp1_preds, aes(competency, id), alpha=0.02) +
  scale_x_continuous(limits=c(0,12*60), breaks=seq(0,12*60,2*60))

predictions3 <- predictions2 |> slice_sample(n=1000)

ggplot() + theme_bw() +
  geom_point(data=predictions3, aes(competency, id), alpha=0.5) +
  scale_x_continuous(limits=c(0,12*60), breaks=seq(0,12*60,2*60))


parameter_draws_log1 <- as_draws_df(event_model_log_1) %>%
  as.data.frame() %>%
  mutate(intercept = (b_Intercept)) %>% # lambda
  select(b_Intercept, sigma)

parameter_draws_log2 <- as_draws_df(event_model_log_2) %>%
  as.data.frame() %>%
  mutate(intercept = (b_Intercept)) %>% # lambda
  select(b_Intercept, sigma)

parameter_draws_log3 <- as_draws_df(event_model_log_3) %>%
  as.data.frame() %>%
  mutate(intercept = (b_Intercept)) %>% # lambda
  select(b_Intercept, sigma)


library(foreach)
nsims=100
n_id=100
dataset_quartiles_log1 <- foreach(i=1:nsims, .combine="rbind") %do% {
  post_sm1_sample <- parameter_draws_log1 %>% slice_sample(n = 1)
  individual_times <- rweibull(runif(n_id), shape = post_sm1_sample[1,2], scale = post_sm1_sample[1,1])
  data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i)) |> mutate(type="log1")
}

dataset_quartiles_log2 <- foreach(i=1:nsims, .combine="rbind") %do% {
  post_sm1_sample <- parameter_draws_log2 %>% slice_sample(n = 1)
  individual_times <- rweibull(runif(n_id), shape = post_sm1_sample[1,2], scale = post_sm1_sample[1,1])
  data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i)) |> mutate(type="log2")
}

dataset_quartiles_log3 <- foreach(i=1:nsims, .combine="rbind") %do% {
  post_sm1_sample <- parameter_draws_log3 %>% slice_sample(n = 1)
  individual_times <- rweibull(runif(n_id), shape = post_sm1_sample[1,2], scale = post_sm1_sample[1,1])
  data.frame(settlement_point=sort(round(individual_times)), id=(n_id)-seq(0,n_id-1,1), sim=(i)) |> mutate(type="log3")
}


datasets <- rbind(dataset_quartiles_log1, dataset_quartiles_log2, dataset_quartiles_log3)


ggplot() +
  theme_bw() +
  xlim(0, 720/60) + facet_wrap(~type) +
  geom_line(data = datasets, aes(settlement_point/60, id, color=type), size=1, alpha=0.8)

