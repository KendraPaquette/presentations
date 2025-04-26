library(tidyverse)
library(tidymodels)
library(tidyposterior)
theme_set(theme_classic()) 

pp <- read_rds("data/posteriors.rds")

pp_tidy <- pp |>  
  tidy(seed = 123)

q = c(.025, .5, .975)
ci <- pp_tidy |>  
  group_by(model) |>  
  summarize(median = quantile(posterior, probs = q[2]),
            lower = quantile(posterior, probs = q[1]), 
            upper = quantile(posterior, probs = q[3])) |>  
  mutate(model = factor(model, levels = c("week", "day", "hour"),
                        labels = c("Week", "Day", "Hour")),
         y = 1000) |> 
  arrange(model)

pp_diffs <- pp |>  
  contrast_models(list("hour","hour", "day"), 
                  list("week", "day", "week"))

ci_diffs <- pp_diffs |> 
  group_by(contrast) |>  
  summarize(median = quantile(difference, probs = q[2]),
            lower = quantile(difference, probs = q[1]), 
            upper = quantile(difference, probs = q[3]))

pp_plot_empty <- pp_tidy |>
  mutate(model = factor(model, levels = c("week", "day", "hour"),
                        labels = c("Week", "Day", "Hour"))) |>
  ggplot() +
  geom_histogram(aes(x = posterior, fill = model), color = "white", alpha = .8,
                 bins = 30) +
  geom_segment(mapping = aes(y = y+100, yend = y-100, x = median, xend = median,
                             color = model), show.legend = FALSE, data = ci) +
  geom_segment(mapping = aes(y = y, yend = y, x = lower, xend = upper, color = model),
               show.legend = FALSE, data = ci) +
  facet_wrap(~model, ncol = 1) +
  scale_y_continuous("Count", breaks = c(0, 500, 1000)) +
  scale_fill_manual(values = c("white", "white", "white")) +
  scale_color_manual(values = c("white", "white", "white")) +
  xlab("Posterior probability for auROC") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())


pp_plot <- pp_tidy |>
  mutate(model = factor(model, levels = c("week", "day", "hour"),
                        labels = c("Week", "Day", "Hour"))) |>
  ggplot() +
  geom_histogram(aes(x = posterior, fill = model), color = "black", alpha = .8,
                 bins = 30) +
  geom_segment(mapping = aes(y = y+100, yend = y-100, x = median, xend = median,
                             color = model), show.legend = FALSE, data = ci) +
  geom_segment(mapping = aes(y = y, yend = y, x = lower, xend = upper, color = model),
               show.legend = FALSE, data = ci) +
  facet_wrap(~model, ncol = 1) +
  scale_y_continuous("Count", breaks = c(0, 500, 1000)) +
  scale_fill_manual(values = c("#b0bec5", "#ef9a9a", "black")) +
  scale_color_manual(values = c("#263238", "#c5050c", "black")) +
  xlab("Posterior probability for auROC") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
