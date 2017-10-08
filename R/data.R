library(dplyr)
library(magrittr)
library(readr)
library(purrr)


.check_duplicates <- function(day_matches) {
  players <- c(day_matches$winner, day_matches$loser)
  dupes <- duplicated(players)
  if (any(dupes)) {
    stop(sprintf("Found players with multiple games on %s: %s",
                 day_matches$date[1],
                 paste(players[dupes], collapse = ', ')))
  }
}


#' Load CSV
#'
#' @param csv_path
#'
#' @return
#' @export
load_results <- function(csv_path) {
  col_types <- cols(
    date = col_date(),
    group = col_character(),
    season = col_integer(),
    winner = col_character(),
    loser = col_character(),
    winner_score = col_number(),
    loser_score = col_number()
  )
  results <- read_csv(csv_path, col_types = col_types)

  required_columns <- names(col_types$cols)
  missed_requirements <- !required_columns %in% colnames(results)
  if (any(missed_requirements)) {
    stop("Missing required columns: ",
         paste(required_columns[missed_requirements], collapse = ', '))
  }

  # Each player can only play one game per date
  results %>% group_by(date) %>% map(~.check_duplicates)
  results
}


#' Melt Match Results
#'
#' Takes in match_results in winner/loser form
#' and turns it into name/opponent form
#'
#' @param match_results
#'
#' @return
#' @export
#'
#' @examples
melt_match_results <- function(match_results) {
  match_results <- match_results %>%
    mutate(pwp = winner_score ^ 2 / (winner_score ^ 2 + loser_score ^ 2))
  flipped <- match_results %>%
    mutate(
      name = loser,
      result = 1 - pwp,
      opponent = winner
    ) %>%
    select(
      name, result, opponent,
      season, date, group)
  match_results %>%
    select(
      name = winner,
      result = pwp,
      opponent = loser,
      season,
      date,
      group
    ) %>%
    bind_rows(flipped)
}
