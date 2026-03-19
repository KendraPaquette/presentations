suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true"))

path_models <- format_path("risk2/models/combined")


read_rds(here::here(path_models, "shapsgrp_1_x_5_day_v10_kfold_full_final.rds")) |> 
  write_rds("data/risk2_shap.rds")





