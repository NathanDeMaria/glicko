library(purrr)

.create_comparison <- function(ratings, previous_ratings, results) {
  # Yep, there's no way to reverse `rank`, so I'm ranking the negative
  this_week <- ratings %>% mutate(rank = rank(-mean))
  last_week <- previous_ratings %>% mutate(rank = rank(-mean))

  week_comparison <- this_week %>%
    inner_join(last_week, by = 'name', suffix = c('_current', '_previous')) %>%
    mutate(rank_change = rank_previous - rank_current,
           mean_change = mean_current - mean_previous)

  scores <- results %>%
    select(player = winner, opponent = loser,
           player_score = winner_score, opponent_score = loser_score) %>%
    bind_rows(results %>%
                select(player = loser, opponent = winner,
                       player_score = loser_score, opponent_score = winner_score))

  # TODO: include bye weeks
  week_comparison %>%
    mutate(player = name) %>%
    inner_join(scores, by = c('player' = 'player'))
}

#' Create comparisons
#'
#' @param all_ratings
#' @param match_results
#'
#' @return
#' @export
#'
#' @examples
create_comparisons <- function(all_ratings, match_results) {
  weekly_ratings <- all_ratings %>% split(all_ratings$date)
  weekly_results <- match_results %>% split(match_results$date)
  weeks <- length(weekly_ratings)

  comparisons <- seq_len(weeks - 1) %>%
    map(~.create_comparison(weekly_ratings[[.x + 1]],
                            weekly_ratings[[.x]],
                            weekly_results[[.x]]))
  names(comparisons) <- weekly_results$date %>% unique()
  comparisons
}
