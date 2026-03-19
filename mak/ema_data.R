suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true"))
suppressPackageStartupMessages(library(tidyposterior))


path_models <- "S:/risk/models/ema"

# Read in data

pp <- read_rds(file.path(path_models, "posteriors_all_0_v5_nested.rds"))

pp_tidy <- pp |>  
  tidy(seed = 123)

q = c(.025, .5, .975)

ci <- pp_tidy |>  
  group_by(model) |>  
  summarize(median = quantile(posterior, probs = q[2]),
            lower = quantile(posterior, probs = q[1]), 
            upper = quantile(posterior, probs = q[3])) |>  
  mutate(model = factor(model, levels = c("week", "day", "hour"),
                        labels = c("Week", "Day", "Hour")),
         y = 1000) |> 
  arrange(model)


ci |> write_rds("data/ema_ci.rds")


