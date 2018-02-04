library(PingPongGlicko)
library(dplyr)
library(magrittr)
library(tibble)
library(purrr)
library(lubridate)
library(readr)

data_path <- 'data/default.csv'
hyper_path <- 'data/hyperparams.RDS'

match_results <- load_results(data_path) %>%
  arrange(date)
hyperparams <- readRDS(hyper_path)

ratings <- do.call(
  get_league_stats,
  c(list(match_results = match_results %>% melt_match_results()),
    hyperparams))$ratings

current_season_players <- match_results %>%
  filter(season == max(season)) %>%
  select(winner, loser) %>% unlist() %>% unique()

ratings <- ratings %>% filter(name %in% current_season_players)

comparisons <- create_comparisons(ratings, match_results)
