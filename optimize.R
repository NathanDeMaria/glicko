library(beepr)
library(PingPongGlicko)
library(tidyverse)
library(lubridate)


.vector_to_args <- function(x, init_params) {
  regression_rate <- if (is.na(init_params$regression_rate)) {
    0
  } else {
    rr <- x[3]
    x <- x[-3]
    rr
  }
  group_diffs <- if(is.null(init_params$group_diffs)) {
    NULL
  } else {
    x[3:length(x)]
  }
  list(
    init_variance = x[1],
    time_variance = x[2],
    regression_rate = regression_rate,
    group_diffs = group_diffs
  )
}

.args_to_vector <- function(args) {
  x <- c(args$init_variance, args$time_variance)
  if(!is.na(args$regression_rate)) {
    x <- c(x, args$regression_rate)
  }
  if(!is.null(args$group_diffs)) {
    x <- c(x, args$group_diffs)
  }
  x
}


optimize_ratings <- function(match_results, init_params) {
  i <- 0
  evaluate_league <- function(x) {
    i <<- i + 1
    cat(sprintf('Iteration: %d\n', i))
    if (i %% 25 == 0) {
      cat(x, '\n')
    }
    arg_list <- .vector_to_args(x, init_params)
    arg_list$match_results <- match_results
    do.call(get_league_stats, arg_list)$discrepancy
  }

  o <- optim(.args_to_vector(init_params), evaluate_league)

  if (o$convergence == 0 & !is.na(o$value)) {
    beep()
  } else {
    beep('coin')
  }
  .vector_to_args(o$par, init_params)
}

league <- 'lincoln'
source(sprintf('load_data/%s.R', league))
rds <- sprintf('load_data/%s.rds', league)
data_file <- sprintf('load_data/%s.csv', league)
match_results <- load_results(data_file) %>%
  melt_match_results() %>%
  arrange(date)

init_params <- if(file.exists(rds)) {
  readRDS(rds)
} else {
  n_groups <- match_results %>%
    filter(season == min(season)) %>%
    .[['group']] %>% unique() %>% length()

  list(
    init_variance = 1000,
    time_variance = 1000,
    regression_rate = 0,
    group_diffs = rep(100, n_groups - 1)
  )
}
par <- optimize_ratings(match_results, init_params)
saveRDS(par, rds)


# Copy over to shiny app defaults
file.copy(rds, 'app/data/hyperparams.RDS', overwrite = T)
file.copy(data_file, 'app/data/default.csv', overwrite = T)

library(shiny)

runApp('app/', launch.browser = T)

