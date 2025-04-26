library(tidyverse)
theme_set(theme_classic()) 

# Read in data

pp_tidy <- read_csv(here::here("data/pp_tidy.csv"), 
                    show_col_types = FALSE) |>
  mutate(model = factor(model, levels = c("No lag", "2 weeks")))

ci <- read_csv(here::here("data/ci.csv"), 
               show_col_types = FALSE) |>
  mutate(model = factor(model, levels = c("No lag", "2 weeks")))

global_all <- read_csv(here::here("data/global_all.csv"),
                       show_col_types = FALSE) |>
  mutate(model = factor(model, levels = c("2 weeks", "No lag")))

local_all <- read_csv(here::here("data/local_all.csv"),
                       show_col_types = FALSE) |> 
  mutate(model = factor(model, levels = c("2 weeks", "No lag")))
  

pp_tidy_dem <- read_csv(here::here("data/pp_tidy_dem.csv"), 
                        show_col_types = FALSE) |>
  mutate(lag = factor(lag, levels = c("No lag", "2 weeks")))

ci_dem <- read_csv(here::here("data/ci_dem.csv"), 
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

# Posterior probabilities Figures

pp_empty <- pp_tidy |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model, color = model), linewidth = .5,  
                 bins = 60) +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               color = "white", linewidth = .5, data = ci |>  filter(model %in% c("No lag")) ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               color = "white", linewidth = .5, data = ci |>  filter(model %in% c("No lag")) ) +
  facet_wrap(~model, ncol = 1) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous("Posterior Probability") +
  scale_fill_manual(values = c("white", "white")) +
  scale_color_manual(values = c("white", "white")) +
  xlab("Area Under ROC Curve") +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20))

pp_no_lag <- pp_tidy |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model, color = model), linewidth = .5,  
                 bins = 60) +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, data = ci |>  filter(model %in% c("No lag")) ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, data = ci |>  filter(model %in% c("No lag")) ) +
  facet_wrap(~model, ncol = 1) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous("Posterior Probability") +
  scale_fill_manual(values = c("#b0bec5", "white")) +
  scale_color_manual(values = c("black", "white")) +
  xlab("Area Under ROC Curve") +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20))

pp_full <- pp_tidy |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model), color = "black", linewidth = .5,  
                 bins = 60) +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, data = ci |>  filter(model %in% c("No lag", "2 weeks")) ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, data = ci |>  filter(model %in% c("No lag", "2 weeks")) ) +
  facet_wrap(~model, ncol = 1) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous("Posterior Probability") +
  scale_fill_manual(values = c("#b0bec5", "#ef9a9a")) +
  xlab("Area Under ROC Curve") +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20))

# Global Shapley plot
shap_levels <- global_all |> 
  mutate(variable_grp = reorder(variable_grp, mean_value, sum)) |>
  pull(variable_grp) |>
  levels()

global_0lag <- global_all |>
  filter(model == "No lag") |> 
  mutate(variable_grp = factor(variable_grp, levels = shap_levels)) |> 
  ggplot() +
  geom_bar(aes(x = variable_grp, y = mean_value, fill = model), 
           stat = "identity", position = "dodge") +
  labs(y = "Mean(|Shapley Value|)",
       x = NULL,
       fill = NULL) +
  scale_fill_manual(values = c("#c5050c", "#263238")) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  coord_flip()

global <- global_all |>
  mutate(variable_grp = factor(variable_grp, levels = shap_levels)) |> 
  ggplot() +
  geom_bar(aes(x = variable_grp, y = mean_value, fill = model), 
           stat = "identity", position = "dodge") +
  labs(y = "Mean(|Shapley Value|)",
       x = NULL,
       fill = NULL) +
  scale_fill_manual(values = c("#c5050c", "#263238")) +
  theme(text = element_text(size = 20),
        legend.position = "right") +
  coord_flip()

# Local Shapley plot
local_0lag <- local_all |> 
  filter(model == "No lag") |> 
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
  theme(legend.position = "none",
        text = element_text(size = 20))+
  geom_hline(aes(yintercept = 0), linetype = "dashed",
             linewidth = .5) +
  scale_y_continuous(limits = c(-2, 6), breaks = seq(-2, 6, 2)) +
  coord_flip()


local <- local_all |> 
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
race_empty <- pp_tidy_dem |>
  filter(lag == "No lag") |> 
  filter(str_detect(group, "Race")) |> 
  mutate(model = factor(model, levels = c("not white", "non-hispanic white"))) |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model), color = "white", linewidth = .5,  
                 bins = 60, fill = "white") +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, color = "white", data = ci_dem |> filter(str_detect(group, "Race") & lag == "No lag") |> 
                 mutate(model = factor(model, levels = c("not white", "non-hispanic white"))) ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, color = "white", data = ci_dem |> filter(str_detect(group, "Race") & lag == "No lag")  |> 
                 mutate(model = factor(model, levels = c("not white", "non-hispanic white"))) ) +
  facet_grid(model~group) +
  scale_y_continuous("Posterior Probability") +
  xlab(NULL) +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text.y = element_blank())

race_no_lag <- pp_tidy_dem |>
  filter(lag == "No lag") |> 
  filter(str_detect(group, "Race")) |> 
  mutate(model = factor(model, levels = c("not white", "non-hispanic white"))) |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model), color = "black", linewidth = .5,  
                 bins = 60, fill = "#b0bec5") +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Race") & lag == "No lag") |> 
                 mutate(model = factor(model, levels = c("not white", "non-hispanic white"))) ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Race") & lag == "No lag")  |> 
                 mutate(model = factor(model, levels = c("not white", "non-hispanic white"))) ) +
  facet_grid(model~group) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous("Posterior Probability") +
  xlab(NULL) +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text.y = element_blank())

race_lag <- pp_tidy_dem |>
  filter(lag == "2 weeks") |> 
  filter(str_detect(group, "Race")) |> 
  mutate(model = factor(model, levels = c("not white", "non-hispanic white"))) |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model), color = "black", linewidth = .5,  
                 bins = 60, fill = "#ef9a9a") +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Race") & lag == "2 weeks") |> 
                 mutate(model = factor(model, levels = c("not white", "non-hispanic white"))) ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Race") & lag == "2 weeks")  |> 
                 mutate(model = factor(model, levels = c("not white", "non-hispanic white"))) ) +
  facet_grid(model~group) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous("Posterior Probability") +
  xlab(NULL) +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text.y = element_blank())

income_empty <- pp_tidy_dem |>
  filter(lag == "No lag") |> 
  filter(str_detect(group, "Income")) |> 
  mutate(model = factor(model, levels = c("below poverty", "above poverty"))) |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model), color = "white", linewidth = .5,  
                 bins = 60, fill = "white") +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, color = "white", data = ci_dem |> filter(str_detect(group, "Income") & lag == "No lag") |> 
                 mutate(model = factor(model, levels = c("below poverty", "above poverty"))) ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, color = "white", data = ci_dem |> filter(str_detect(group, "Income") & lag == "No lag")  |> 
                 mutate(model = factor(model, levels = c("below poverty", "above poverty"))) ) +
  facet_grid(model~group) +
  scale_y_continuous(NULL) +
  xlab("Area Under ROC Curve") +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank())

income_no_lag <- pp_tidy_dem |>
  filter(lag == "No lag") |> 
  filter(str_detect(group, "Income")) |> 
  mutate(model = factor(model, levels = c("below poverty", "above poverty"))) |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model), color = "black", linewidth = .5,  
                 bins = 60, fill = "#b0bec5") +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Income") & lag == "No lag") |> 
                 mutate(model = factor(model, levels = c("below poverty", "above poverty"))) ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Income") & lag == "No lag")  |> 
                 mutate(model = factor(model, levels = c("below poverty", "above poverty"))) ) +
  facet_grid(model~group) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous(NULL) +
  xlab("Area Under ROC Curve") +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank())

income_lag <- pp_tidy_dem |>
  filter(lag == "2 weeks") |> 
  filter(str_detect(group, "Income")) |> 
  mutate(model = factor(model, levels = c("below poverty", "above poverty"))) |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model), color = "black", linewidth = .5,  
                 bins = 60, fill = "#ef9a9a") +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Income") & lag == "2 weeks") |> 
                 mutate(model = factor(model, levels = c("below poverty", "above poverty"))) ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Income") & lag == "2 weeks")  |> 
                 mutate(model = factor(model, levels = c("below poverty", "above poverty"))) ) +
  facet_grid(model~group) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous(NULL) +
  xlab("Area Under ROC Curve") +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank())

sex_empty <- pp_tidy_dem |>
  filter(lag == "No lag") |> 
  filter(str_detect(group, "Sex")) |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model), color = "white", linewidth = .5,  
                 bins = 60, fill = "white") +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, color = "white", data = ci_dem |> filter(str_detect(group, "Sex") & lag == "No lag") ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, color = "white", data = ci_dem |> filter(str_detect(group, "Sex") & lag == "No lag")  ) +
  facet_grid(model~group) +
  scale_y_continuous(NULL) +
  xlab(NULL) +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank())

sex_no_lag <- pp_tidy_dem |>
  filter(lag == "No lag") |> 
  filter(str_detect(group, "Sex")) |> 
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model), color = "black", linewidth = .5,  
                 bins = 60, fill = "#b0bec5") +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Sex") & lag == "No lag") ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Sex") & lag == "No lag")  ) +
  facet_grid(model~group) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous(NULL) +
  xlab(NULL) +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank())

sex_lag <- pp_tidy_dem |>
  filter(lag == "2 weeks") |> 
  filter(str_detect(group, "Sex")) |>
  ggplot() + 
  geom_histogram(aes(x = posterior, fill = model), color = "black", linewidth = .5,  
                 bins = 60, fill = "#ef9a9a") +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Sex") & lag == "2 weeks") ) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, data = ci_dem |> filter(str_detect(group, "Sex") & lag == "2 weeks")  ) +
  facet_grid(model~group) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous(NULL) +
  xlab(NULL) +
  expand_limits(x = c(.5, 1)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank())


pp_tidy <- read_csv(here::here("data/lag_pp_tidy.csv"), 
                    show_col_types = FALSE) 

ci <- read_csv(here::here("data/lag_ci.csv"), 
               show_col_types = FALSE) |> 
  mutate(model = factor(model, levels = c("No lag", "1 day", "3 days", "1 week", "2 weeks")))

lag_posteriors <- pp_tidy |> 
  mutate(model = factor(model, levels = c("No lag", "1 day", "3 days", "1 week", "2 weeks"))) |>
  ggplot() + 
  geom_histogram(aes(x = posterior), fill = "light grey", color = "black", linewidth = .5,  
                 bins = 60) +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, data = ci) +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, data = ci) +
  facet_wrap(~model, ncol = 1) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous("Posterior Probability") +
  xlab("Area Under ROC Curve") +
  expand_limits(x = c(.5, 1)) +
  theme_classic() 

lag_posteriors_empty <- pp_tidy |> 
  mutate(model = factor(model, levels = c("No lag", "1 day", "3 days", "1 week", "2 weeks"))) |>
  ggplot() + 
  geom_histogram(aes(x = posterior), fill = "white", color = "white", linewidth = .5,  
                 bins = 60) +
  geom_segment(mapping = aes(y = 3400, yend = 3800, x = pp_median, xend = pp_median),
               linewidth = .5, data = ci, color = "white") +
  geom_segment(mapping = aes(y = 3600, yend = 3600, x = pp_lower, xend = pp_upper),
               linewidth = .5, data = ci, color = "white") +
  facet_wrap(~model, ncol = 1) +
  geom_vline(xintercept = .5, linewidth = .5, linetype = "dashed") +
  scale_y_continuous("Posterior Probability") +
  xlab("Area Under ROC Curve") +
  expand_limits(x = c(.5, 1)) +
  theme_classic() 
