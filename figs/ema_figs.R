library(tidyverse)
library(tidymodels)
library(tidyposterior)
theme_set(theme_classic()) 

ci <- read_rds("data/ema_ci.rds")


# Performance
perf_ema_empty <- ci |>
  ggplot(aes(x = model, y = median,  color = model)) + 
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_segment(mapping = aes(x = model, y = lower, yend = upper, color = model),
               position = position_dodge(width = 0.5), linewidth = 1) +
  scale_y_continuous("auROC", limits = c(.50, 1.0)) +
  labs(x = NULL,
       color = NULL) +
  theme_classic() +
  theme(legend.position =  "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_fill_manual(values = c("white", "white", "white")) +
  scale_color_manual(values = c("white", "white", "white")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  theme(text = element_text(size = 20))

perf_ema <- ci |>
  ggplot(aes(x = model, y = median,  color = model)) + 
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_segment(mapping = aes(x = model, y = lower, yend = upper, color = model),
               position = position_dodge(width = 0.5), linewidth = 1) +
  scale_y_continuous("auROC", limits = c(.50, 1.0)) +
  labs(x = NULL,
       color = NULL) +
  theme_classic() +
  theme(legend.position =  "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("black", "black", "black")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  theme(text = element_text(size = 20))

perf_ema_red <- ci |>
  ggplot(aes(x = model, y = median,  color = model)) + 
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_segment(mapping = aes(x = model, y = lower, yend = upper, color = model),
               position = position_dodge(width = 0.5), linewidth = 1) +
  scale_y_continuous("auROC", limits = c(.50, 1.0)) +
  labs(x = NULL,
       color = NULL) +
  theme_classic() +
  theme(legend.position =  "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#263238", "#c5050c", "#263238")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  theme(text = element_text(size = 20))

