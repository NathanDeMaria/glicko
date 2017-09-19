library(dplyr)


# Found via optimise(evaluate_league, c(150^2, 300^2))
.init_variance <- 30950.75


#' Add players
#'
#' @param ratings
#' @param player_groups
#'
#' @return
#' @export
add_players <- function(all_ratings, player_groups) {
  group_ratings <- all_ratings %>%
    filter_most_recent() %>%
    inner_join(player_groups, by = 'name') %>%
    group_by(group) %>%
    summarise(
      mean = mean(mean),
      variance = .init_variance)


  # Ratings for new players
  new_ratings <- player_groups %>%
    filter(!name %in% all_ratings$name) %>%
    inner_join(group_ratings, by = 'group') %>%
    mutate(date = max(all_ratings$date),
           wins = 0,
           losses = 0) %>%
    select(-group)

  bind_rows(all_ratings, new_ratings)
}


#' Create initial ratings
#'
#' @return
#' @export
create_initial_ratings <- function(player_groups,
                                   init_variance = .init_variance,
                                   group_diffs = NULL,
                                   init_time = 0) {
  group_names <- unique(player_groups$group)
  if (is.null(group_diffs)) {
    group_diffs <- rep(0, max(length(group_names) - 1, 1))
  }
  offsets <- cumsum(c(0, group_diffs))
  group_priors <- 1500 - offsets + mean(offsets)
  names(group_priors) <- group_names

  player_groups %>%
    mutate(mean = unlist(group_priors[group]),
           variance = init_variance,
           # TODO: pick a more general time unit here
           date = init_time,
           wins = 0,
           losses = 0) %>%
    select(-group)
}
