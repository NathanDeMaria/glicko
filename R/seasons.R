library(dplyr)
library(purrr)


.q <- log(10) / 400


#' Win Probability
#'
#' @param matchup
#'
#' @return
#' @export
#'
#' @examples
calc_win_probability <- function(matchup) {
  variance_combined <- matchup$player_variance + matchup$opponent_variance
  rating_diff <- matchup$player_mean - matchup$opponent_mean
  1 / (1 + 10 ^ (-.g(variance_combined) * rating_diff / 400))
}



#' Find matchup
#'
#' @param ratings
#' @param player
#' @param opponent
#'
#' @return
#' @export
#'
#' @examples
matchup <- function(ratings, player, opponent) {
  player_stats <- ratings %>% filter(name == player) %>%
    mutate(player_mean = mean, player_variance = variance)
  opp_stats <- ratings %>% filter(name == opponent) %>%
    mutate(opponent_mean = mean, opponent_variance = variance)
  cbind(player_stats, opp_stats) %>% calc_win_probability()
}


.run_week <- function(current_week, ratings) {
  week_with_ratings <- current_week %>% left_join(ratings, by = c('winner' = 'name')) %>%
    left_join(ratings, by = c('loser' = 'name'), suffix = c('.winner', '.loser'))
  winner_stack <- week_with_ratings %>%
    select(
      name = winner, result = pwp,
      player_mean = mean.winner, player_variance = variance.winner,
      opponent_mean = mean.loser, opponent_variance = variance.loser)
  loser_stack <- week_with_ratings %>%
    mutate(result = 1 - pwp) %>%
    select(
      name = loser, result,
      player_mean = mean.loser, player_variance = variance.loser,
      opponent_mean = mean.winner, opponent_variance = variance.winner)
  named_results <-  bind_rows(winner_stack, loser_stack)

  # See http://www.glicko.net/research/glicko.pdf for math
  g_opp <- .g(named_results$opponent_variance)
  exp_result <- 1 / (1 + 10^(-g_opp * (named_results$player_mean - named_results$opponent_mean) / 400))
  delta_sq <- 1 / (.q^2 * g_opp^2 * exp_result * (1 - exp_result))
  denom <- 1 / named_results$player_variance + 1 / delta_sq
  new_mean <- named_results$player_mean + .q / denom * g_opp * (named_results$result - exp_result)
  new_variance <- 1 / denom
  ratings <- named_results %>% mutate(new_mean, new_variance) %>%
    right_join(ratings, by = 'name') %>%
    # If a player didn't play this week, use the previous ratings
    mutate(mean = if_else(is.na(new_mean), mean, new_mean),
           variance = if_else(is.na(new_variance), variance, new_variance)) %>%
    select(name, mean, variance)

  p_ij <- calc_win_probability(named_results)
  discrepancy <- -named_results$result * log(p_ij) - (1 - named_results$result) * log(1 - p_ij)

  list(ratings = ratings, discrepancy = sum(discrepancy))
}

.g <- function(variance) {
  1 / sqrt(1 + 3 * (.q ^ 2) * variance / (pi ^ 2))
}

# Used in case people played 2 games in the same week
.split_week <- function(current_week) {
  # TODO: be better
  names_so_far <- c()
  first_indices <- c()
  second_indices <- c()
  for (i in seq_len(nrow(current_week))) {
    game <- current_week[i,]
    if (game$winner %in% names_so_far | game$loser %in% names_so_far) {
      second_indices <- c(second_indices, i)
    } else {
      names_so_far <- c(names_so_far, game$winner, game$loser)
      first_indices <- c(first_indices, i)
    }
  }
  if (length(second_indices) == 0) {
    return(list(current_week))
  }
  list(current_week[first_indices,], current_week[second_indices,])
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
  weeks <- current_season %>%
    split(current_season$week) %>%
    map(.split_week) %>%
    unlist(recursive = F)
  for (current_week in weeks) {
    week_result <- .run_week(current_week, ratings)
    ratings <- week_result$ratings
    discrepancy <- discrepancy + week_result$discrepancy
  }
  list(ratings = ratings, discrepancy = discrepancy)
}


.find_player_groups <- function(match_results) {
  # TODO: sanity check to make sure there's not duplicates?
  match_results %>%
    distinct(winner, league_group) %>%
    select(name = winner, group = league_group) %>%
    full_join(
      match_results %>%
        distinct(loser, league_group) %>%
        select(name = loser, group = league_group),
      by = c('name', 'group'))
}


#' Get league stats
#'
#'
#' @return
#' @export
#'
#' @examples
get_league_stats <- function(match_results, init_variance, group_diffs) {
  discrepancy <- 0
  seasons <- match_results %>% split(match_results$season)
  ratings <- create_initial_ratings(
    .find_player_groups(seasons[[1]]),
    init_variance,
    group_diffs)
  season_result <- run_season(seasons[[1]], ratings)
  ratings <- season_result$ratings
  discrepancy <- discrepancy + season_result$discrepancy
  for (current_season in seasons[-1]) {
    # Find the group each player is in for this season
    player_groups <- .find_player_groups(current_season)
    ratings <- add_players(ratings, player_groups)
    season_result <- run_season(current_season, ratings)
    ratings <- season_result$ratings
    discrepancy <- discrepancy + season_result$discrepancy
  }
  list(ratings = ratings, discrepancy = discrepancy)
}
