library(brms)
library(tidybayes)
library(splines)
library(modelr)


mc_larv <- read.csv("mc_larv_culture.csv") |> 
  pivot_longer(Day.0:Day.6)%>% 
  mutate(name = str_remove(name, "Day\\.")) |> 
  mutate(day=as.numeric(name), larvae=as.numeric(value)) |> na.omit()



mc_model <- brm(formula =  larvae ~ ns(day,3) + (1|Pool),
                          data = mc_larv |> filter(scenario=="scenario1"), 
                        #  family = lognormal(link = "identity", link_sigma = "log").
                          warmup = 500, 
                          iter = 2000, 
                          chains = 4, 
                          init= "0", 
                          cores=8)



mc_larv %>%
  #group_by(cyl) %>%
  data_grid(day = seq_range(day, n = 6), Pool=unique(Pool)) %>%
  add_epred_draws(mc_model) %>%
  ggplot(aes(x = day, y = larvae)) + #, color = ordered(cyl))) +
  stat_lineribbon(aes(y = .epred)) +
  geom_point(data = mc_larv) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") +
  theme_bw()
