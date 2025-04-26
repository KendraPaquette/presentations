suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true"))
suppressPackageStartupMessages(library(tidyposterior))

path_shared <- "/Volumes/private/studydata/risk/data_processed/shared"
path_ema<- "/Volumes/private/studydata/risk/data_processed/ema"
path_models <- "/Volumes/jjcurtin/studydata/risk/models/ema"

# Read in data

pp <- read_rds(file.path(path_models, "posteriors_all_0_v5_nested.rds"))

pp |> write_rds("data/posteriors.rds")


