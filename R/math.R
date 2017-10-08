.q <- log(10) / 400

.g <- function(variance) {
  1 / sqrt(1 + 3 * (.q ^ 2) * variance / (pi ^ 2))
}

#' Win Probability
#'
#' @param matchup
#'
#' @return
#' @export
#'
#' @examples
calc_win_probability <- function(matchup) {
  variance_combined <- matchup$variance + matchup$variance_opponent
  rating_diff <- matchup$mean - matchup$mean_opponent
  1 / (1 + 10 ^ (-.g(variance_combined) * rating_diff / 400))
}


#' Update Ratings
#'
#' @param player_mean
#' @param player_variance
#' @param opponent_mean
#' @param opponent_variance
#' @param result
#'
#' @return
#' @export
#'
#' @examples
update_ratings <- function(player_mean, player_variance,
                           opponent_mean, opponent_variance,
                           result) {
  g_opp <- .g(opponent_variance)
  exp_result <- 1 / (1 + 10^(-g_opp * (player_mean - opponent_mean) / 400))
  delta_sq <- 1 / (.q^2 * g_opp^2 * exp_result * (1 - exp_result))
  denom <- 1 / player_variance + 1 / delta_sq
  new_mean <- player_mean + .q / denom * g_opp * (result - exp_result)
  new_variance <- 1 / denom
  dplyr::tibble(new_mean, new_variance)
}
