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
  named_results <- ratings %>%
    left_join(current_day, by = 'name') %>%
    # TODO: filter to only opponent mean/var?
    left_join(ratings, by = c('opponent' = 'name'), suffix = c('', '_opponent'))

  # See http://www.glicko.net/research/glicko.pdf for math
  new_ratings <- update_ratings(named_results$mean, named_results$variance,
                                named_results$mean_opponent, named_results$variance_opponent,
                                named_results$result)

  next_ratings <- named_results %>%
    cbind(new_ratings) %>%
    mutate(
      win_update = if_else(is.na(result), 0, as.numeric(result > 0.5)),
      loss_update = if_else(is.na(result), 0, as.numeric(result < 0.5))
      # TODO: TIES???
    ) %>%
    mutate(
      mean = if_else(is.na(new_mean), mean, new_mean),
      variance = if_else(is.na(new_variance), variance, new_variance),
      wins = wins + win_update,
      losses = losses + loss_update
    ) %>%
    select(name, mean, variance, wins, losses) %>%
    mutate(date = current_day$date[1])

  p_ij <- calc_win_probability(named_results)
  discrepancy <- -named_results$result * log(p_ij) - (1 - named_results$result) * log(1 - p_ij)

  list(ratings = bind_rows(all_ratings, next_ratings),
       discrepancy = sum(discrepancy, na.rm = T))
}

#' Run season
#'
#' @param current_season
#' @param ratings
#'
#' @return
#' @export
run_season <- function(current_season, ratings) {
  current_season %>%
    split(.$date) %>%
    purrr::reduce(
      function(state, current_day) {
        day_result <- .run_day(current_day, state$ratings)
        list(
          ratings = day_result$ratings,
          discrepancy = state$discrepancy + day_result$discrepancy
        )
      },
      .init = list(discrepancy = 0, ratings = ratings)
    )
}


.find_player_groups <- function(match_results) {
  # TODO: sanity check to make sure there's not duplicates?
  match_results %>%
    distinct(name, group) %>%
    select(name, group)
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
  seasons <- match_results %>%
    split(match_results$season)
  all_ratings <- create_initial_ratings(
    .find_player_groups(seasons[[1]]),
    init_variance,
    group_diffs,
    init_time = min(match_results$date) - days(1))
  initial_season_result <- run_season(seasons[[1]], all_ratings)

  seasons[-1] %>%
    purrr::reduce(
      function(state, current_season) {
        # Find the group each player is in for this season
        player_groups <- .find_player_groups(current_season)
        all_ratings <- state$ratings %>%
          .add_variance(time_variance) %>%
          add_players(player_groups)
        season_result <- run_season(current_season, all_ratings)
        list(
          ratings = season_result$ratings,
          discrepancy = state$discrepancy + season_result$discrepancy
        )
      },
      .init = initial_season_result
    )
}
