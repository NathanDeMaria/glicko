library(dplyr)
library(magrittr)
library(readr)
library(purrr)


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
  melted <- match_results %>%
    select(
      name = winner,
      result = pwp,
      opponent = loser,
      season,
      date,
      group
    ) %>%
    bind_rows(flipped)
  # Extra assert that each player played at most 1 game in each week
  duplicates <- melted %>%
    group_by(name, date) %>%
    summarise(count = n()) %>%
    filter(count > 1)
  if (nrow(duplicates) > 0) {
    print(duplicates)
    stop("Players played multiple games in the same week!")
  }
  melted
}
