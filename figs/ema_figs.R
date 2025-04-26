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
                 bins = 60) +
  geom_segment(mapping = aes(y = 2100, yend = 1900, x = median, xend = median,
                             color = model), show.legend = FALSE, data = ci) +
  geom_segment(mapping = aes(y = 2000, yend = 2000, x = lower, xend = upper, color = model),
               show.legend = FALSE, data = ci) +
  facet_wrap(~model, ncol = 1) +
  scale_y_continuous("Count", breaks = c(0, 500, 1000, 1500)) +
  scale_fill_manual(values = c("white", "white", "white")) +
  scale_color_manual(values = c("white", "white", "white")) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  expand_limits(x = c(.5, 1)) +
  xlab("Posterior probability for auROC") +
  theme(legend.position = "none")

pp_plot <- pp_tidy |>
  mutate(model = factor(model, levels = c("week", "day", "hour"),
                        labels = c("Week", "Day", "Hour"))) |>
  ggplot() +
  geom_histogram(aes(x = posterior, fill = model), color = "black", alpha = .8,
                 bins = 60) +
  geom_segment(mapping = aes(y = 2100, yend = 1900, x = median, xend = median,
                             color = model), show.legend = FALSE, data = ci) +
  geom_segment(mapping = aes(y = 2000, yend = 2000, x = lower, xend = upper, color = model),
               show.legend = FALSE, data = ci) +
  facet_wrap(~model, ncol = 1) +
  scale_y_continuous("Count", breaks = c(0, 500, 1000, 1500)) +
  scale_fill_manual(values = c("#b0bec5", "#ef9a9a", "#b0bec5")) +
  scale_color_manual(values = c("#263238", "#c5050c", "#263238")) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  expand_limits(x = c(.5, 1)) +
  xlab("Posterior probability for auROC") +
  theme(legend.position = "none")

