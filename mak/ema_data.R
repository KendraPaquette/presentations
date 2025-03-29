suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true"))
suppressPackageStartupMessages(library(tidyposterior))

path_models_lag <- "/Volumes/private/studydata/risk/models/lag"
path_shared <- "/Volumes/private/studydata/risk/data_processed/shared"
path_ema<- "/Volumes/private/studydata/risk/data_processed/ema"


pp_tidy <- read_csv(here::here(path_models_lag, "posteriors.csv"), 
                    show_col_types = FALSE) |> 
  mutate(model = factor(model, levels = c("lag0", "lag24", "lag72", "lag168", "lag336"),
                        labels = c("No lag", "1 day", "3 days", "1 week", "2 weeks"))) |>
  filter(model %in% c("No lag", "2 weeks")) 

ci <- read_csv(here::here(path_models_lag, "pp_perf_tibble.csv"), 
               show_col_types = FALSE) |> 
  mutate(model = factor(model, levels = c("0 lag", "24 lag", "72 lag", "168 lag", "336 lag"),
                        labels = c("No lag", "1 day", "3 days", "1 week", "2 weeks")))  |> 
  filter(model %in% c("2 weeks", "No lag"))

global_all <- read_rds(here::here(path_models_lag, "shap_global_all.rds")) |> 
  filter(str_detect(variable_grp, "EMA")) |> 
  mutate(variable_grp = str_remove(variable_grp, "\\(EMA item\\)"),
         variable_grp = reorder(variable_grp, mean_value, sum),
         model = factor(model, levels = c("336 lag", "168 lag", "72 lag", "24 lag", "0 lag"),
                        labels = c("2 weeks", "1 week", "3 days", "1 day", "No lag" ))) |> 
  filter(model %in% c("2 weeks", "No lag"))



pp_tidy_dem <- read_csv(here::here(path_models_lag, "posteriors_dem.csv"), 
                        show_col_types = FALSE) |> 
  mutate(lag = factor(lag, levels = c(0, 336),
                      labels = c("No lag", "2 weeks")),
         model = factor(model)) |> 
  mutate(group = case_when(model %in% c("female", "male") ~ "Sex (female, male)",
                           model %in% c("not white", "non-hispanic white") ~ "Race (not White, White)",
                           model %in% c("below poverty", "above poverty") ~ "Income (below poverty, above poverty)")) |> 
  filter(!is.na(lag))

ci_dem <- read_csv(here::here(path_models_lag, "pp_dem_all.csv"), 
                   show_col_types = FALSE) |> 
  mutate(lag = factor(lag, levels = c(0, 336),
                      labels = c("No lag", "2 weeks")),
         model = factor(model)) |> 
  mutate(group = case_when(model %in% c("female", "male") ~ "Sex (female, male)",
                           model %in% c("not white", "non-hispanic white") ~ "Race (not White, White)",
                           model %in% c("below poverty", "above poverty") ~ "Income (below poverty, above poverty)")) |> 
  filter(!is.na(lag))

shap_feat_0 <- read_rds(here::here(path_models_lag, 
                                   "outer_shapsgrp_1day_0_v3_nested_main.rds")) |> 
  filter(str_detect(variable_grp, "EMA")) |> 
  mutate(variable_grp = str_remove(variable_grp, "\\(EMA item\\)")) |> 
  group_by(variable_grp) |> 
  summarize(min(value), max(value)) |> 
  mutate(model = "No lag")

shap_feat_336 <- read_rds(here::here(path_models_lag, 
                                     "outer_shapsgrp_1day_336_v3_nested_main.rds")) |> 
  filter(str_detect(variable_grp, "EMA")) |> 
  mutate(variable_grp = str_remove(variable_grp, "\\(EMA item\\)"))  |> 
  group_by(variable_grp) |> 
  summarize(min(value), max(value)) |> 
  mutate(model = "2 weeks")

shap_feat_0 |>
  bind_rows(shap_feat_336) |> 
  rename(min = `min(value)`,
         max = `max(value)`) |> 
  write_csv("data/local_all.csv")


write_csv(pp_tidy, "data/pp_tidy.csv")
write_csv(ci, "data/ci.csv")
write_csv(global_all, "data/global_all.csv")
write_csv(pp_tidy_dem, "data/pp_tidy_dem.csv")
write_csv(ci_dem, "data/ci_dem.csv")


# EMA compliance
visit_dates <- read_csv(file.path(path_shared, "visit_dates.csv"), col_types = cols())
ema_m <- vroom::vroom(file.path(path_shared, "ema_morning.csv"), col_types = vroom::cols()) %>% 
  mutate(start_date = with_tz(start_date, tzone = "America/Chicago"),
         subid = as.numeric(subid))
ema_l <- vroom::vroom(file.path(path_shared, "ema_later.csv"), col_types = vroom::cols()) %>% 
  mutate(start_date = with_tz(start_date, tzone = "America/Chicago"),
         subid = as.numeric(subid))

disposition <- read_csv(file.path(path_ema, "disposition.csv"), col_types = cols())

# function to map over
get_study_days <- function(the_subid, dates) {
  start_study <- dates %>% filter(subid == the_subid) %>% pull(start_study)
  end_study <- dates %>% filter(subid == the_subid) %>% pull(end_study)
  study_days <- tibble(subid = the_subid, study_day = seq(start_study, end_study - days(1), by = "day")) 
  return(study_days)
}

sample <- disposition %>% 
  filter(analysis == "yes")

subids <- sample$subid
dates <- sample %>% 
  select(subid, start_study, end_study)

study_dates <- subids %>% 
  map_dfr(~get_study_days(.x, dates))

ema <- ema_m |> 
  select(subid, start_date) |> 
  full_join(ema_l |> select(subid, start_date), by = c("subid", "start_date")) |> 
  mutate(start_date = date(start_date)) |> 
  filter(subid %in% subids)

# count EMAs per day
ema_count_4x <- ema |>  
  count(subid, start_date) |>
  mutate(n = if_else(n > 4, 4, as.numeric(n)))

# left join with study dates
ema_study_dates <- study_dates |>
  left_join(ema_count_4x, by = c("subid", "study_day" = "start_date")) |> 
  mutate(n = if_else(is.na(n), 0, n)) |> 
  mutate(n_prompts = 4)

# slice into 7 day bins
ema_study_weeks <- ema_study_dates |> 
  group_by(subid) |> 
  slice(1:7) |> 
  mutate(week = 1) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(8:14) |> 
              mutate(week = 2)) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(15:21) |> 
              mutate(week = 3)) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(22:28) |> 
              mutate(week = 4)) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(29:35) |> 
              mutate(week = 5)) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(36:42) |> 
              mutate(week = 6)) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(43:49) |> 
              mutate(week = 7)) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(50:56) |> 
              mutate(week = 8)) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(57:63) |> 
              mutate(week = 9)) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(64:70) |> 
              mutate(week = 10)) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(71:77) |> 
              mutate(week = 11)) |> 
  bind_rows(ema_study_dates |> 
              group_by(subid) |> 
              slice(78:84) |> 
              mutate(week = 12)) |> 
  ungroup()

ema_week_compliance_4x <- ema_study_weeks |> 
  group_by(subid, week) |> 
  summarize(sum_n = sum(n), sum_prompts = sum(n_prompts), .groups = "rowwise") |> 
  mutate(compliance = sum_n/sum_prompts) |> 
  ungroup()

ema_week_compliance_1x <- ema_study_weeks |>
  mutate(n = if_else(n > 1, 1, n),
         n_prompts = 1) |> 
  group_by(subid, week) |> 
  summarize(sum_n = sum(n), sum_prompts = sum(n_prompts), .groups = "rowwise") |> 
  mutate(compliance = sum_n/sum_prompts) |> 
  ungroup()

week_compliance_all <- ema_week_compliance_4x |> 
  mutate(compliance = compliance*100) |> 
  group_by(week) |> 
  summarize(mean_compliance = mean(compliance),
            n = n(),
            sd = sd(compliance)) |> 
  mutate(se = sd/sqrt(n),
         signal = "4x Daily") |> 
  bind_rows(ema_week_compliance_1x |> 
              mutate(compliance = compliance*100) |> 
              group_by(week) |> 
              summarize(mean_compliance = mean(compliance),
                        n = n(),
                        sd = sd(compliance)) |> 
              mutate(se = sd/sqrt(n),
                     signal = "1x Daily")) 

write_csv(week_compliance_all, "data/week_compliance_all.csv")

