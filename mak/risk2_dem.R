suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true"))

path_shared <- format_path("studydata/risk2/data_processed/shared")
path_ema <- format_path("studydata/risk2/data_processed/ema")

study_dates <- read_csv(here::here(path_ema, "study_dates.csv"),
                        show_col_types = FALSE) 

intake <- read_csv(here::here(path_shared, "survey_intake.csv"),
                   show_col_types = FALSE)  |> 
  filter(subid %in% study_dates$subid)

race <- intake |> 
  select(subid, starts_with("race")) |> 
  pivot_longer(cols = starts_with("race")) |>
  filter(name != "race_other_text") |>
  filter(value == "yes")  |> 
  select(subid, race = name) |> 
  mutate(race = str_remove(race, "race_")) 

subs_1race <- race |> 
  group_by(subid) |> 
  count() |> 
  filter(n == 1)

subs_white_only <- race |> 
  filter(subid %in% subs_1race$subid & race == "white")

race <- race |> 
  mutate(race = if_else(subid %in% subs_white_only$subid, "White only", race)) |> 
  filter(!race == "white") |> 
  mutate(race = case_match(race,
                           "White only" ~ "White only",
                           "ai_an" ~ "American Indian or Alaska Native",
                           "asian" ~ "Asian",
                           "black" ~ "Black",
                           "hispanic" ~ "Hispanic",
                           "nat_hi" ~ "Native Hawaiin or Pacific Islander",
                           "other" ~ "Race not listed"))

oud <- intake |> 
  select(subid, dsm_c)

# confirm matches sex at birth
sex <- intake |> 
  select(subid, gender) |> 
  rename(sex = gender) |> 
  mutate(sex = case_when(sex == "Man" ~ "male",
                         sex == "Woman" ~ "female",
                         TRUE ~ NA_character_)) |> 
  filter(!is.na(sex))

age <- intake |> 
  select(subid, age)

income <- intake |> 
  select(subid, income) |> 
  filter(!is.na(income)) |> 
  mutate(income = case_match(income,
                             "Less than $25,000" ~ "0-24",
                             "$25,000 - $34, 999" ~ "25-34",
                             "$35,000 - $49,999" ~ "35-49",
                             "$50,000 - $74, 999" ~ "50-74",
                             "$75, 000 - $99, 999" ~ "75-99",
                             "$100,000 - $149,999" ~ "100+",
                             "$150, 000 - $199,999" ~ "100+",
                             "$200, 000 or more" ~ "100+"))

write_csv(race, "data/risk2_race.csv")

write_csv(oud, "data/risk2_oud.csv")

write_csv(sex, "data/risk2_sex.csv")

write_csv(age, "data/risk2_age.csv")

write_csv(income, "data/risk2_income.csv")

