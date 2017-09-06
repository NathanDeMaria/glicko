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
  49986.84486,
  11738.58529,
  c(254.90184, 155.56518, 224.80542, 235.56013, 51.97142))$ratings %>%
  arrange(desc(mean))
