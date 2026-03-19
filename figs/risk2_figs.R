library(tidyverse)
library(lubridate)
theme_set(theme_classic()) 

# Read in data
compliance <- read_csv("data/risk2_adherence.csv",
                       show_col_types = FALSE)

shaps <- read_rds("data/risk2_shap.rds") 

pp_dem_risk2 <- read_csv("data/risk2_pp_dem.csv", show_col_types = FALSE)

ci_risk2 <- read_csv(here::here("data/risk2_ci.csv"), 
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


# Feature importance
shap_levels <- shaps |> 
  group_by(variable_grp) |>
  summarize(mean_value = mean(abs(value)), .groups = "drop") |> 
  arrange(desc(mean_value)) |> 
  slice_head(n = 30) |> 
  arrange(mean_value) |> 
  pull(variable_grp)

n_obs <- max(shaps$id_obs)

shaps_day_max <- shaps |>
  group_by(id_obs) |>
  slice_max(value) |> 
  group_by(variable_grp) |> 
  summarise(n = n(),
            prop = n/n_obs) |> 
  ungroup() 


global_risk2 <- shaps |>
  group_by(variable_grp) |>
  summarize(mean_value = mean(abs(value)), .groups = "drop") |> 
  filter(variable_grp %in% shap_levels) |> 
  mutate(variable_grp = factor(variable_grp, levels = shap_levels)) |> 
  ggplot(mapping = aes(x = variable_grp, y = mean_value)) +
  geom_bar(fill = "#c5050c", 
           stat = "identity", position = "dodge") +
  labs(y = "Mean(|Shapley Value|)",
       x = NULL,
       fill = NULL) +
  theme_classic() +
  theme(axis.text=element_text(size=9.5),
        legend.key.size = unit(0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position = "right") +
  coord_flip()


global_risk2_v2 <- shaps_day_max |> 
  filter(variable_grp %in% shap_levels) |>
  mutate(variable_grp = factor(variable_grp, levels = shap_levels)) |>
  ggplot(mapping = aes(x = variable_grp, y = prop)) +
  geom_bar(fill = "#263238", 
           stat = "identity", position = "dodge") +
  labs(y = "Proportion of days as top feature",
       x = NULL,
       fill = NULL) +
  theme_classic() +
  theme(axis.text=element_text(size=9.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key.size = unit(0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position = "right") +
  coord_flip()

local_risk2 <- shaps|>
  filter(variable_grp %in% c("Past opioid use, Diff (EMA)", "Urge, Raw (EMA)", "Abstinence confidence, Raw (EMA)")) |>
  mutate(variable_grp = factor(variable_grp, levels = c("Past opioid use, Diff (EMA)", "Urge, Raw (EMA)", "Abstinence confidence, Raw (EMA)"))) |>
  ggplot(aes(x = rfvalue, y = value)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5, bs = "cs"), se = FALSE) +
  facet_wrap(~ variable_grp, scales = "free", ncol = 3) +
  labs(
    title = "SHAP Variable Relationships",
    x = "z-score raw feature score",
    y = "Shapley value"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold")
  )

# fairness
risk2_fair <- pp_dem_risk2 |>
  mutate(group = case_match(model,
                            "male" ~ "Gender",
                            "not male" ~ "Gender",
                            "non-Hispanic White" ~ "Race",
                            "Hispanic and/or not white" ~ "Race",
                            "below poverty" ~ "Income",
                            "above poverty" ~ "Income",
                            "urban/suburban" ~ "Geography",
                            "small town/rural" ~ "Geography",
                            "heterosexual" ~ "Orientation",
                            "not heterosexual" ~ "Orientation"),
         fairness = if_else(model %in% c("not male", 
                                         "Hispanic and/or not white",
                                         "below poverty",
                                         "small town/rural",
                                         "not heterosexual"), 
                            "Disadvantaged (not male, Hispanic and/or not White, <$25,000 income, rural, not heterosexual)",
                            "Advantaged (male, non-Hispanic White, $25,000+ income, urban, heterosexual)"),
         fairness = factor(fairness),
         group = factor(group, levels = c("Gender", "Race",
                                          "Income", "Orientation", "Geography"))) |>  
  ggplot(aes(x = group, y = pp_median, color = fairness)) + 
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_segment(mapping = aes(x = group, y = pp_lower, yend = pp_upper, color = fairness),
               position = position_dodge(width = 0.5), linewidth = 1) +
  scale_y_continuous("auROC", limits = c(.50, 1.0)) +
  labs(x = NULL,
       color = NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#263238", "#c5050c")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0)


# performance
risk2_perf_all <- ci_risk2 |>
  mutate(model = factor(model, levels = c("full", "ablated geolocation", "ablated daily survey",  "ablated daily survey and geolocation"),
                        labels = c("full", "ablated geolocation", "ablated EMA", "ablated EMA and geolocation"))) |>
  ggplot(aes(x = model, y = pp_median,  color = model)) + 
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_segment(mapping = aes(x = model, y = pp_lower, yend = pp_upper, color = model),
               position = position_dodge(width = 0.5), linewidth = 1) +
  scale_y_continuous("auROC", limits = c(.50, 1.0)) +
  labs(x = NULL,
       color = NULL) +
  theme_classic() +
  theme(legend.position =  "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#c5050c", "#263238", "#263238", "#263238")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0)


risk2_perf_1 <- ci_risk2 |>
  mutate(model = factor(model, levels = c("full", "ablated geolocation", "ablated daily survey",  "ablated daily survey and geolocation"),
                        labels = c("full", "ablated GPS", "ablated EMA", "ablated EMA and GPS"))) |>
  ggplot(aes(x = model, y = pp_median,  color = model)) + 
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_segment(mapping = aes(x = model, y = pp_lower, yend = pp_upper, color = model),
               position = position_dodge(width = 0.5), linewidth = 1) +
  scale_y_continuous("auROC", limits = c(.50, 1.0)) +
  labs(x = NULL,
       color = NULL) +
  theme_classic() +
  theme(legend.position =  "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#c5050c", "white", "white", "white")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0)
