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
  select(subid, starts_with("race"))


write_csv(race, "data/risk2_race.csv")

