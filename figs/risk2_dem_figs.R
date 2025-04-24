library(tidyverse)
theme_set(theme_classic()) 

# Read in data

oud <- read_csv(here::here("data/risk2_oud.csv"), 
                    show_col_types = FALSE) 
race <- read_csv(here::here("data/risk2_race.csv"), 
                show_col_types = FALSE) 
sex <- read_csv(here::here("data/risk2_sex.csv"), 
                show_col_types = FALSE) 

age <- read_csv(here::here("data/risk2_age.csv"), 
                show_col_types = FALSE) 

income <- read_csv(here::here("data/risk2_income.csv"), 
                show_col_types = FALSE) 

fig_oud <- oud |>
  filter(!is.na(dsm_c)) |> 
  ggplot(aes(x = dsm_c)) +
  geom_histogram(binwidth = 1, bins = 8, fill = "#263238", color = "white") +
  labs(title = "OUD Severity", x = "Number of OUD Symptoms", y = "Count") +
  scale_x_continuous(breaks = seq(0, 11),
                     limits = c(-.5, 11.5)) +
  theme(text = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5))

fig_race <- race |>
  mutate(race = if_else(race == "hispanic", "hisp", race)) |> 
  ggplot(aes(x = race)) +
  geom_bar(stat = "count", fill = "#263238") +
  ggtitle(NULL) +
  labs(title = "Race", x = NULL, y = "Count") +
  theme(text = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5))

fig_sex <- sex |>
  mutate(group = sex) |> 
  ggplot(aes(x = group)) +
  geom_bar(stat = "count",  fill = "#263238") +
  ggtitle(NULL) +
  labs(title = "Sex", x = NULL, y = "Count") +
  theme(text = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5))

fig_age <- age |>
  ggplot(aes(x = age)) +
  geom_bar(stat = "count",  fill = "#263238") +
  ggtitle(NULL) +
  theme(text = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5))

fig_income <- income |>
  mutate(income = factor(income, levels = c("0-24", "25-34", "35-49",
                                            "50-74", "75-99", "100+"))) |> 
  ggplot(aes(x = income)) +
  geom_bar(stat = "count",  fill = "#263238") +
  ggtitle(NULL) +
  xlab("Income (in thousands)") +
  theme(text = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5))
