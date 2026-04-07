library(tidyverse)
theme_set(theme_classic()) 

# Read in data


ci_lag <- read_csv(here::here("data/lag_ci.csv"), 
               show_col_types = FALSE) 

shap <- read_csv(here::here("data/lag_shap.csv"),
                       show_col_types = FALSE) |>
  mutate(model = factor(model, levels = c("2 weeks", "No lag")))

shap_risk1 <- read_rds(here::here("data/risk1_shap.rds"))

shap_risk1_336 <- read_rds(here::here("data/risk1_shap_336.rds"))


ci_dem <- read_csv(here::here("data/lag_ci_dem.csv"), 
                   show_col_types = FALSE) |>
  mutate(lag = factor(lag, levels = c("No lag", "2 weeks")))

week_compliance_all <- read_csv(here::here("data/week_compliance_all.csv"),
                                show_col_types = FALSE)



# Compliance Figure
fig_compliance <- week_compliance_all |> 
  group_by(week, signal) |> 
  ggplot(aes(x = week, y = mean_compliance, group = signal, color = signal, fill = signal)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_compliance - se, ymax = mean_compliance + se), alpha = .4) +
  theme_classic() +
  scale_color_manual(values = c("#263238", "#c5050c")) +
  scale_fill_manual(values = c("#b0bec5", "#ef9a9a")) +
  scale_x_continuous(name = "Week", 
                     breaks = seq(1, 12, 1)) +
  scale_y_continuous(name = "Completion percentage", 
                     breaks = seq(0, 100, 10), 
                     limits = c(0, 100)) +
  geom_hline(aes(yintercept = mean_compliance, color = signal), week_compliance_all |> 
               group_by(signal) |> 
               summarize(mean_compliance = mean(mean_compliance)),
             linetype = "dashed", linewidth = .3) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

# Performance
perf_lag <- ci_lag |>
  mutate(model = factor(model, levels = c("No lag", "1 day", "3 days", "1 week", "2 weeks"))) |>
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
        text = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#c5050c", "#263238", "#263238", "#263238", "#263238")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0)
  
perf_lag_empty <- ci_lag |>
  mutate(model = factor(model, levels = c("No lag", "1 day", "3 days", "1 week", "2 weeks"))) |>
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
        text = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#c5050c", "white", "white", "white", "white")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0)

  
# Global Shapley plot
shap_levels <- shap |> 
  mutate(variable_grp = reorder(variable_grp, mean_value, sum)) |>
  pull(variable_grp) |>
  levels()

shap_levels_0lag <- shap |> 
  filter(model == "No lag") |> 
  mutate(variable_grp = reorder(variable_grp, mean_value, sum)) |>
  pull(variable_grp) |>
  levels()

global_0lag <- shap |>
  filter(model == "No lag") |> 
  mutate(variable_grp = factor(variable_grp, levels = shap_levels_0lag)) |> 
  ggplot() +
  geom_bar(aes(x = variable_grp, y = mean_value, fill = model), 
           stat = "identity", position = "dodge") +
  labs(y = "Mean(|Shapley Value|)",
       x = NULL,
       fill = NULL) +
  scale_fill_manual(values = c("#c5050c")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(text = element_text(size = 20),
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  coord_flip()

n_obs <- max(shap_risk1$id_obs)

shaps_day_max <- shap_risk1 |>
  group_by(id_obs) |>
  slice_max(value) |> 
  group_by(variable_grp) |> 
  summarise(n = n(),
            prop = n/n_obs) |> 
  ungroup() 


shaps_day_max <- shaps_day_max |> 
  filter(!str_detect(variable_grp, "demographic")) |>
  filter(!str_detect(variable_grp, "other")) |>
  mutate(variable_grp = str_remove(variable_grp, " \\(EMA item\\)"))

global_0lag_v2 <- shaps_day_max |> 
  filter(variable_grp %in% shap_levels) |>
  mutate(variable_grp = factor(variable_grp, levels = shap_levels)) |>
  ggplot(mapping = aes(x = variable_grp, y = prop)) +
  geom_bar(fill = "#263238", 
           stat = "identity", position = "dodge") +
  labs(y = "Proportion of days as top feature",
       x = NULL,
       fill = NULL) +
  theme_classic() +
  theme(text = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position = "right") +
  coord_flip()


n_obs <- max(shap_risk1_336$id_obs)

shaps_day_max_336 <- shap_risk1_336 |>
  group_by(id_obs) |>
  slice_max(value) |> 
  group_by(variable_grp) |> 
  summarise(n = n(),
            prop = n/n_obs) |> 
  ungroup() 


shaps_day_max <- shaps_day_max |> 
  mutate(model = "No lag") |>
  bind_rows(shaps_day_max_336 |> 
              mutate(model = "2 weeks") |> 
              filter(!str_detect(variable_grp, "demographic")) |> 
              filter(!str_detect(variable_grp, "other")) |>
              mutate(variable_grp = str_remove(variable_grp, " \\(EMA item\\)"))) |> 
  mutate(model = factor(model, levels = c("2 weeks", "No lag")))

global <- shap |>
  mutate(variable_grp = factor(variable_grp, levels = shap_levels)) |> 
  ggplot() +
  geom_bar(aes(x = variable_grp, y = mean_value, fill = model), 
           stat = "identity", position = "dodge") +
  labs(y = "Mean(|Shapley Value|)",
       x = NULL,
       fill = NULL) +
  scale_fill_manual(values = c("#263238", "#c5050c")) +
  theme(text = element_text(size = 20),
        legend.position = "right",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  coord_flip()

global_no_legend <- shap |>
  mutate(variable_grp = factor(variable_grp, levels = shap_levels)) |> 
  ggplot() +
  geom_bar(aes(x = variable_grp, y = mean_value, fill = model), 
           stat = "identity", position = "dodge") +
  labs(y = "Mean(|Shapley Value|)",
       x = NULL,
       fill = NULL) +
  scale_fill_manual(values = c("#263238", "#c5050c")) +
  theme(text = element_text(size = 20),
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  coord_flip()

global_v2 <- shaps_day_max |>
  mutate(variable_grp = factor(variable_grp, levels = shap_levels)) |> 
  ggplot(mapping = aes(x = variable_grp, y = prop)) +
  geom_bar(aes(fill = model), stat = "identity", position = "dodge") +
    labs(y = "Proportion of days as top feature",
         x = NULL,
         fill = NULL) +
  scale_fill_manual(values = c("#263238", "#c5050c")) +
  theme(text = element_text(size = 20),
        legend.position = "right",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  coord_flip()




# Local Shapley plot
local_0lag <- shap |> 
  filter(model == "No lag") |> 
  mutate(variable_grp = factor(variable_grp, levels = shap_levels_0lag)) |> 
  ggplot() +
  geom_segment(aes(x = variable_grp, y = min, yend = max, group = model,
                   color = model),
               position = position_dodge(width = .5), 
               linewidth = 1) +
  ylab("Raw Shapley Value") +
  xlab(NULL) +
  labs(color = NULL) +
  scale_color_manual(values = c("#c5050c", "#263238")) +
  theme(legend.position = "none",
        text = element_text(size = 20))+
  geom_hline(aes(yintercept = 0), linetype = "dashed",
             linewidth = .5) +
  scale_y_continuous(limits = c(-2, 6), breaks = seq(-2, 6, 2)) +
  coord_flip()


local <- shap |> 
  mutate(variable_grp = factor(variable_grp, levels = shap_levels)) |> 
  ggplot() +
  geom_segment(aes(x = variable_grp, y = min, yend = max, group = model,
                   color = model),
               position = position_dodge(width = .5), 
               linewidth = 1) +
  ylab("Raw Shapley Value") +
  xlab(NULL) +
  labs(color = NULL) +
  scale_color_manual(values = c("#c5050c", "#263238")) +
  theme(legend.position = "right",
        text = element_text(size = 20))+
  geom_hline(aes(yintercept = 0), linetype = "dashed",
             linewidth = .5) +
  scale_y_continuous(limits = c(-2, 6), breaks = seq(-2, 6, 2)) +
  coord_flip()


# Fairness

ci_dem |>
  mutate(group = factor(model, 
                        levels = c("Race (not White, White)", "Income (below poverty, above poverty)", "Sex (female, male)"),
                        labels = c("Race/ethnicity", "Income", "Sex")),
         fairness = if_else(model %in% c("female", 
                                         "not white",
                                         "below poverty"), 
                            "Disadvantaged (female, Hispanic and/or not White, <$25,000 income)",
                            "Advantaged (male, non-Hispanic White, $25,000+ income)"),
         fairness = factor(fairness)) |>
  ggplot(aes(x = group, y = pp_median, color = fairness, group = fairness)) + 
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_segment(mapping = aes(x = group, y = pp_lower, yend = pp_upper, color = fairness, group = fairness),
               position = position_dodge(width = 0.5),
               linewidth = 1) +
  scale_y_continuous("auROC", limits = c(.50, 1.0)) +
  labs(x = NULL,
       color = NULL) +
  theme_classic() +
  theme(legend.position =  "none",
        text = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#c5050c", "#263238")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


fair_0lag_all <- ci_dem |> 
  filter(lag == "No lag") |> 
  mutate(group = case_match(model,
                          "male" ~ "Sex",
                          "female" ~ "Sex",
                          "non-hispanic white" ~ "Race",
                          "not white" ~ "Race",
                          "below poverty" ~ "Income",
                          "above poverty" ~ "Income"),
       fairness = if_else(model %in% c("female", 
                                       "not white",
                                       "below poverty"), 
                          "Disadvantaged (Hispanic and/or not White, <$25,000, female)",
                          "Advantaged (non-Hispanic White, $25,000+, male)"),
       fairness = factor(fairness),
       group = factor(group, levels = c("Race",
                                        "Income", "Sex"))) |>  
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
        text = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#263238", "#c5050c")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

fair_0lag_empty <- ci_dem |> 
  filter(lag == "No lag") |> 
  mutate(group = case_match(model,
                            "male" ~ "Sex",
                            "female" ~ "Sex",
                            "non-hispanic white" ~ "Race",
                            "not white" ~ "Race",
                            "below poverty" ~ "Income",
                            "above poverty" ~ "Income"),
         fairness = if_else(model %in% c("female", 
                                         "not white",
                                         "below poverty"), 
                            "Disadvantaged (Hispanic and/or not White, <$25,000, female)",
                            "Advantaged (non-Hispanic White, $25,000+, male)"),
         fairness = factor(fairness),
         group = factor(group, levels = c("Race",
                                          "Income", "Sex"))) |>  
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
        text = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("white", "white")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

fair_0lag_2 <- ci_dem |> 
  filter(lag == "No lag") |>
  mutate(group = case_match(model,
                            "male" ~ "Sex",
                            "female" ~ "Sex",
                            "non-hispanic white" ~ "Race",
                            "not white" ~ "Race",
                            "below poverty" ~ "Income",
                            "above poverty" ~ "Income"),
         fairness = if_else(model %in% c("female", 
                                         "not white",
                                         "below poverty"), 
                            "Disadvantaged (Hispanic and/or not White, <$25,000, female)",
                            "Advantaged (non-Hispanic White, $25,000+, male)"),
         fairness = factor(fairness),
         group = factor(group, levels = c("Race",
                                          "Income", "Sex"))) |> 
  filter(group != "Sex") |> 
  ggplot(aes(x = group, y = pp_median, color = fairness)) + 
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_segment(mapping = aes(x = group, y = pp_lower, yend = pp_upper, color = fairness),
               position = position_dodge(width = 0.5), linewidth = 1) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous("auROC", limits = c(.50, 1.0)) +
  labs(x = NULL,
       color = NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#263238", "#c5050c")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


fair_336lag_all <- ci_dem |> 
  filter(lag == "2 weeks") |> 
  mutate(group = case_match(model,
                            "male" ~ "Sex",
                            "female" ~ "Sex",
                            "non-hispanic white" ~ "Race",
                            "not white" ~ "Race",
                            "below poverty" ~ "Income",
                            "above poverty" ~ "Income"),
         fairness = if_else(model %in% c("female", 
                                         "not white",
                                         "below poverty"), 
                            "Disadvantaged (Hispanic and/or not White, <$25,000, female)",
                            "Advantaged (non-Hispanic White, $25,000+, male)"),
         fairness = factor(fairness),
         group = factor(group, levels = c("Race",
                                          "Income", "Sex"))) |>  
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
        text = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#263238", "#c5050c")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

fair_336lag_empty <- ci_dem |> 
  filter(lag == "2 weeks") |> 
  mutate(group = case_match(model,
                            "male" ~ "Sex",
                            "female" ~ "Sex",
                            "non-hispanic white" ~ "Race",
                            "not white" ~ "Race",
                            "below poverty" ~ "Income",
                            "above poverty" ~ "Income"),
         fairness = if_else(model %in% c("female", 
                                         "not white",
                                         "below poverty"), 
                            "Disadvantaged (Hispanic and/or not White, <$25,000, female)",
                            "Advantaged (non-Hispanic White, $25,000+, male)"),
         fairness = factor(fairness),
         group = factor(group, levels = c("Race",
                                          "Income", "Sex"))) |>  
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
        text = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("white", "white")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

fair_336lag_2 <- ci_dem |> 
  filter(lag == "2 weeks") |>
  mutate(group = case_match(model,
                            "male" ~ "Sex",
                            "female" ~ "Sex",
                            "non-hispanic white" ~ "Race",
                            "not white" ~ "Race",
                            "below poverty" ~ "Income",
                            "above poverty" ~ "Income"),
         fairness = if_else(model %in% c("female", 
                                         "not white",
                                         "below poverty"), 
                            "Disadvantaged (Hispanic and/or not White, <$25,000, female)",
                            "Advantaged (non-Hispanic White, $25,000+, male)"),
         fairness = factor(fairness),
         group = factor(group, levels = c("Race",
                                          "Income", "Sex"))) |> 
  filter(group != "Sex") |> 
  ggplot(aes(x = group, y = pp_median, color = fairness)) + 
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_segment(mapping = aes(x = group, y = pp_lower, yend = pp_upper, color = fairness),
               position = position_dodge(width = 0.5), linewidth = 1) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous("auROC", limits = c(.50, 1.0)) +
  labs(x = NULL,
       color = NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  scale_color_manual(values = c("#263238", "#c5050c")) +
  geom_hline(yintercept = 1.0, linetype = "dashed",  color = "#263238") +
  geom_hline(yintercept = 0.5, linetype = "dashed",  color = "#263238") +
  geom_label(aes(x = 1, y = 1.0, label = "Perfect"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  geom_label(aes(x = 1, y = 0.5, label = "Chance"),
             inherit.aes = FALSE, fill = "white", color = "#263238", label.size = 0) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))



