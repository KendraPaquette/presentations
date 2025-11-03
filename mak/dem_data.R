suppressPackageStartupMessages(library(tidyverse))

path_processed <- "/Volumes/jjcurtin/studydata/risk/data_processed/ema"
path_shared <- "/Volumes/jjcurtin/studydata/risk/data_processed/shared"


disposition <- read_csv(file.path(path_processed, "disposition.csv"), 
                        col_types = "ccDDcccccccccc")
screen <- read_csv(file.path(path_shared, "screen.csv"), 
                   col_types = cols()) |>
  filter(subid %in% subset(disposition, analysis == "yes")$subid) |> 
  mutate(across(dsm5_1:dsm5_11, ~ if_else(.x == "Yes", 1, 0))) |>  
  rowwise() |>  
  mutate(aud_total = sum(c(dsm5_1, dsm5_2, dsm5_3, dsm5_4, dsm5_5, dsm5_6, dsm5_7, 
                           dsm5_8, dsm5_9, dsm5_10, dsm5_11))) |> 
  ungroup() |> 
  select(subid, age = dem_1, sex = dem_2, race = dem_3, ethnicity = dem_4, educ = dem_5, 
         income = dem_7, ms = dem_8, aud_total)  |> 
  mutate(educ = case_match(educ, 
                           "2-Year degree" ~ "Some college",
                           "High school or GED" ~ "<= High school", 
                           "Less than high school or GED degree" ~ "<= High school",
                           "College degree" ~ "4 year degree",
                           .default = educ),
         race = case_match(race,
                           "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
                           "Asian" ~ "Asian",
                           "Black/African American" ~ "Black",
                           "Other/Multiracial" ~ "Multiracial or race not listed",
                           "White/Caucasian" ~ "Non-Hispanic White")) 

race <- screen |> 
  select(subid, race, ethnicity) |> 
  mutate(ethnicity = if_else(str_detect(ethnicity, "Yes"), "Hispanic", NA_character_)) |> 
  pivot_longer(cols = race:ethnicity, values_to = "race") |> 
  select(-name) |> 
  filter(!is.na(race))


write_csv(screen, "data/dem.csv")

write_csv(race, "data/risk1_race.csv")

