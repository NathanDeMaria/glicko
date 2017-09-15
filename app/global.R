library(PingPongGlicko)
library(dplyr)
library(magrittr)
library(tibble)
library(purrr)
library(lubridate)

data_path <- 'data/default.csv'

match_results <- load_results(data_path) %>%
  mutate(pwp = winner_sets ^ 2 / (winner_sets ^ 2 + loser_sets ^ 2)) %>%
  arrange(week)

ratings <- get_league_stats(
  match_results,
  48585.88830,
  12196.90082,
  c(255.09608, 157.29326, 222.52522, 231.20889, 50.33347))$ratings
