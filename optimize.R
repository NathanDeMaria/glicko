library(beepr)
library(PingPongGlicko)
library(tidyverse)
library(lubridate)


optimize_ratings <- function(data_file, init_params) {
  match_results <- load_results(data_file) %>%
    melt_match_results() %>%
    arrange(date)

  evaluate_league <- function(x) {
    get_league_stats(match_results, x[1], x[2], x[3], x[4])$discrepancy
  }

  o <- optim(init_params, evaluate_league)

  if(o$convergence == 0) {
    beep()
  } else {
    beep('wilhelm')
  }
  o$par
}

save_params <- function(par, rds) {
  params <- list(
    init_variance = par[1],
    time_variance = par[2],
    regression_rate = par[3],
    group_diffs = par[4])
  saveRDS(params, file = rds)
}


data_file <- 'load_data/ncaa.csv'
init_ratings <- c(2.005950e+04, 8.684297e+03, 3.414318e-01, NULL)
par <- optimize_ratings(data_file, init_ratings)
save_params(par, 'load_data/ncaa.rds')
