library(PingPongGlicko)
library(dplyr)
library(magrittr)
library(tibble)
library(purrr)
library(lubridate)
library(readr)

data_path <- 'data/default.csv'

match_results <- load_results(data_path) %>% arrange(date)

ratings <- get_league_stats(
  match_results,
  14892.584, 9704.322,
  NULL)$ratings
