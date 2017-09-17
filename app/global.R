library(PingPongGlicko)
library(dplyr)
library(magrittr)
library(tibble)
library(purrr)
library(lubridate)

data_path <- 'data/default.csv'

match_results <- load_results(data_path) %>% arrange(date)

ratings <- get_league_stats(
  match_results,
  47971.90567,
  11329.37511,
  c(255.46227, 155.17675, 216.95251, 229.24910, 47.12791))$ratings
