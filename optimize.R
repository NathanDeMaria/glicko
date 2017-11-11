library(beepr)
library(PingPongGlicko)
library(tidyverse)
library(lubridate)


optimize_ratings <- function(data_file, init_params) {
  match_results <- load_results(data_file) %>%
    melt_match_results() %>%
    arrange(date)

  evaluate_league <- function(x) {
    get_league_stats(match_results, x[1], x[2], x[3], NULL)$discrepancy
  }

  o <- optim(init_params, evaluate_league)

  if (o$convergence == 0 & !is.na(o$value)) {
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
    group_diffs = NULL)
  saveRDS(params, file = rds)
}


data_file <- 'load_data/ncaa.csv'
init_params <- c(2.006160e+04, 1.068920e+04, -2.573196e-02)
par <- optimize_ratings(data_file, init_params)
save_params(par, 'load_data/ncaa.rds')
