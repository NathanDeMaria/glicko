library(dplyr)
library(lubridate)
library(magrittr)
library(readr)


#' Load CSV
#'
#' @param csv_path
#'
#' @return
#' @export
load_results <- function(csv_path) {
  results <- readr::read_csv(csv_path)

  required_columns <- c('week', 'league_group', 'season', 'winner', 'loser', 'winner_sets', 'loser_sets')
  missed_requirements <- !required_columns %in% colnames(results)
  if(any(missed_requirements)) {
    stop("Missing required columns: ", required_columns[missed_requirements])
  }
  results %>% dplyr::mutate(week = lubridate::ymd(week)) %>% filter(!is.na(winner))
}
