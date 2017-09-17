context("Basic season")

test_that("Simple run_season runs", {
  current_season <- tibble(
    winner = 'Peyton',
    loser = 'Tom',
    pwp = 1,
    date = as.Date('1989-12-13')
  )
  ratings <- tibble(
    name = c('Peyton', 'Tom'),
    mean = c(1700, 1300),
    variance = c(100, 100),
    date = as.Date('1989-12-13'),
    wins = c(100, 0),
    losses = c(0, 100)
  )
  season_result <- run_season(current_season, ratings)

  expect_is(season_result$ratings, 'tbl_df')
  expect_gt(season_result$discrepancy, 0)
})
