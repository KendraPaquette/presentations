library(tidyverse)
theme_set(theme_classic()) 

# Read in data
compliance <- read_csv("data/risk2_ema_adherence.csv",
                       show_col_types = FALSE)


# Compliance Figure
fig_risk2_compliance <- compliance |> 
  mutate(adherence = adherence*100) |> 
  group_by(month) |> 
  summarize(mean_compliance = mean(adherence),
            n = n(),
            sd = sd(adherence)) |> 
  mutate(se = sd/sqrt(n)) |> 
  ggplot(aes(x = month, y = mean_compliance, color = "#c5050c", fill = "#ef9a9a")) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_compliance - se, ymax = mean_compliance + se), alpha = .4) +
  theme_classic() +
  scale_x_continuous(name = "Month", 
                     breaks = seq(1, 12, 1)) +
  scale_y_continuous(name = "Completion percentage", 
                     breaks = seq(0, 100, 10), 
                     limits = c(0, 100)) +
  geom_hline(aes(yintercept = mean_compliance, color = "#c5050c"), compliance |> 
               summarize(mean_compliance = mean(adherence)*100),
             linetype = "dashed", linewidth = .3) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 20))

