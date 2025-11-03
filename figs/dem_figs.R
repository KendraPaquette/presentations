library(tidyverse)
theme_set(theme_classic()) 

# Read in data

dem <- read_csv(here::here("data/dem.csv"), 
                    show_col_types = FALSE) 
race <- read_csv(here::here("data/risk1_race.csv"), 
                show_col_types = FALSE) 

fig_aud <- dem |>
  ggplot(aes(x = aud_total)) +
  geom_histogram(binwidth = 1, bins = 8, fill = "#263238", color = "white") +
  labs(title = "AUD Severity", x = "Number of AUD Symptoms", y = "Count") +
  scale_x_continuous(breaks = seq(0, 11),
                     limits = c(0, 11.5)) +
  theme(text = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5))

fig_race <- race |>
  mutate(race = factor(race, levels = c("Multiracial or race not listed",
                                        "Hispanic",
                                        "Black",
                                        "Asian",
                                        "American Indian or Alaska Native",
                                        "Non-Hispanic White"))) |> 
  ggplot(aes(x = race)) +
  geom_bar(stat = "count", fill = "#263238") +
  ggtitle(NULL) +
  labs(title = "Race", x = NULL, y = "Count") +
  theme(text = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

fig_sex <- dem |>
  mutate(group = sex) |> 
  ggplot(aes(x = group)) +
  geom_bar(stat = "count",  fill = "#263238") +
  ggtitle(NULL) +
  labs(title = "Sex", x = NULL, y = "Count") +
  theme(text = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5))

fig_age <- dem |>
  ggplot(aes(x = age)) +
  geom_histogram(fill = "#263238", color = "white", bins = 20) +
  labs(title = "Age", x = "Age", y = "Count") +
  geom_vline(xintercept = mean(dem$age), linewidth = 1, color = "#c5050c") +
  theme(text = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5))

fig_income <- dem |>
  ggplot(aes(x = income)) +
  geom_histogram(fill = "#263238", color = "white", bins = 20) +
  labs(title = "Income", x = "Income", y = "Count") +
  geom_vline(xintercept = mean(dem$income), linewidth = 1, color = "#c5050c") +
  theme(text = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5))
