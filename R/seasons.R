library(dplyr)
library(lubridate)
library(purrr)


#' Filter to current
#'
#' @param all_ratings
#'
#' @return
#' @export
#'
#' @examples
filter_most_recent <- function(all_ratings) {
  all_ratings %>% filter(date == max(date))
}


#' Find a head to head win probability
#'
#' @param ratings
#' @param player
#' @param opponent
#'
#' @return
#' @export
#'
#' @examples
matchup <- function(all_ratings, player, opponent) {
  ratings <- all_ratings %>% filter_most_recent()
  player_stats <- ratings %>% filter(name == player) %>%
    mutate(player_mean = mean, player_variance = variance)
  opp_stats <- ratings %>% filter(name == opponent) %>%
    mutate(opponent_mean = mean, opponent_variance = variance)
  cbind(player_stats, opp_stats) %>% calc_win_probability()
}


.run_day <- function(current_day, all_ratings) {
  ratings <- all_ratings %>% filter_most_recent()
  day_with_ratings <- current_day %>% left_join(ratings, by = c('winner' = 'name')) %>%
    left_join(ratings, by = c('loser' = 'name'), suffix = c('.winner', '.loser'))
  winner_stack <- day_with_ratings %>%
    select(
      name = winner, result = pwp,
      player_mean = mean.winner, player_variance = variance.winner,
      opponent_mean = mean.loser, opponent_variance = variance.loser,
      wins = wins.winner, losses = losses.winner) %>%
    mutate(wins = wins + 1)
  loser_stack <- day_with_ratings %>%
    mutate(result = 1 - pwp) %>%
    select(
      name = loser, result,
      player_mean = mean.loser, player_variance = variance.loser,
      opponent_mean = mean.winner, opponent_variance = variance.winner,
      wins = wins.loser, losses = losses.loser) %>%
    mutate(losses = losses + 1)

  named_results <- bind_rows(winner_stack, loser_stack)

  # See http://www.glicko.net/research/glicko.pdf for math
  new_ratings <- update_ratings(named_results$player_mean, named_results$player_variance,
                                named_results$opponent_mean, named_results$opponent_variance,
                                named_results$result)
  next_ratings <- named_results %>% cbind(new_ratings) %>%
    right_join(ratings, by = 'name') %>%
    # If a player didn't play this day, use the previous ratings
    mutate(mean = if_else(is.na(new_mean), mean, new_mean),
           variance = if_else(is.na(new_variance), variance, new_variance),
           date = current_day$date[1],
           wins = pmax(wins.x, wins.y, na.rm = TRUE),
           losses = pmax(losses.x, losses.y, na.rm = TRUE)) %>%
    select(name, mean, variance, date, wins, losses)

  p_ij <- calc_win_probability(named_results)
  discrepancy <- -named_results$result * log(p_ij) - (1 - named_results$result) * log(1 - p_ij)

  list(ratings = bind_rows(all_ratings, next_ratings),
       discrepancy = sum(discrepancy))
}

#' Run season
#'
#' @param current_season
#' @param ratings
#'
#' @return
#' @export
run_season <- function(current_season, ratings) {
  discrepancy <- 0
  day_tables <- current_season %>% split(.$date)
  for (current_day in day_tables) {
    day_result <- .run_day(current_day, ratings)
    ratings <- day_result$ratings
    discrepancy <- discrepancy + day_result$discrepancy
  }
  list(ratings = ratings, discrepancy = discrepancy)
}


.find_player_groups <- function(match_results) {
  # TODO: sanity check to make sure there's not duplicates?
  match_results %>%
    distinct(winner, group) %>%
    select(name = winner, group) %>%
    full_join(
      match_results %>%
        distinct(loser, group) %>%
        select(name = loser, group),
      by = c('name', 'group'))
}


.add_variance <- function(all_ratings, time_variance) {
  # TODO: match up w/ filter_most_recent
  most_recent <- all_ratings$date == max(all_ratings$date)
  all_ratings$variance[most_recent] <- all_ratings$variance[most_recent] + time_variance
  all_ratings
}


#' Get league stats
#'
#'
#' @return
#' @export
#'
#' @examples
get_league_stats <- function(match_results, init_variance, time_variance, group_diffs) {
  discrepancy <- 0
  seasons <- match_results %>%
    mutate(pwp = winner_score ^ 2 / (winner_score ^ 2 + loser_score ^ 2)) %>%
    split(match_results$season)
  all_ratings <- create_initial_ratings(
    .find_player_groups(seasons[[1]]),
    init_variance,
    group_diffs,
    init_time = min(match_results$date) - days(1))
  season_result <- run_season(seasons[[1]], all_ratings)
  all_ratings <- season_result$ratings
  discrepancy <- discrepancy + season_result$discrepancy
  for (current_season in seasons[-1]) {
    # Find the group each player is in for this season
    player_groups <- .find_player_groups(current_season)
    all_ratings <- .add_variance(all_ratings, time_variance)
    all_ratings <- add_players(all_ratings, player_groups)
    season_result <- run_season(current_season, all_ratings)
    all_ratings <- season_result$ratings
    discrepancy <- discrepancy + season_result$discrepancy
  }
  list(ratings = all_ratings, discrepancy = discrepancy)
}
