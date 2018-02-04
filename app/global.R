library(PingPongGlicko)
library(dplyr)
library(magrittr)
library(tibble)
library(purrr)
library(lubridate)
library(readr)
library(ggvis)

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

comparisons <- create_comparisons(ratings, match_results) %>%
  # Compute some of the display data now.
  mutate(
    rank_sign = ifelse(
      rank_change > 0, '↑',
      ifelse(rank_change < 0, '↓', '↔')),
    result = ifelse(
      player_score > opponent_score,
      'Win', ifelse(player_score < opponent_score, 'Loss', 'Tie')),
    mean_change_text = ifelse(
      mean_change > 0,
      sprintf('+%.01f', mean_change),
      ifelse(mean_change < 0, sprintf('-%.01f', abs(mean_change)), '↔'))
  ) %>%
  mutate(
    tooltip_text = sprintf(
      "%s<br/>%s vs %s %d-%d<br/>%s (%s %.0f)",
      date_current, result, opponent,
      player_score, opponent_score,
      mean_change_text, rank_sign, rank_change)
    # TODO: NA if NA
  )
