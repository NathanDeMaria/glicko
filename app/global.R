library(PingPongGlicko)
library(dplyr)
library(magrittr)
library(tibble)
library(purrr)
library(lubridate)
library(readr)

data_path <- 'data/default.csv'

match_results <- load_results(data_path) %>%
  arrange(date)

ratings <- get_league_stats(
  match_results %>% melt_match_results(),
  2.005950e+04, 8.684297e+03, 3.414318e-01,
  NULL)$ratings

comparisons <- create_comparisons(ratings, match_results)
