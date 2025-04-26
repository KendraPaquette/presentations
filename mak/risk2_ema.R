suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true"))

path_shared <- format_path("studydata/risk2/data_processed/shared")
path_ema <- format_path("studydata/risk2/data_processed/ema")

study_dates <- read_csv(here::here(path_ema, "study_dates.csv"),
                        show_col_types = FALSE) 


ema <-read_csv(here::here(path_shared, "survey_daily.csv"),
                 show_col_types = FALSE) 

lapse <- read_csv(here::here(path_shared, "survey_lapses.csv"),
                 show_col_types = FALSE) 


write_csv(ema |> 
            select(subid, complete_date), "data/risk2_ema_completion_times.csv")

write_csv(lapse |> 
            select(subid, lapse_dttm, utc_offset), "data/risk2_lapse_times.csv")



