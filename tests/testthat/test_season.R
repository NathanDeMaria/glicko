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


test_that("Season with byes", {
  current_season <- tibble(
    winner = c('Peyton', 'Drew', 'Peyton'),
    loser = c('Tom', 'Aaron', 'Drew'),
    pwp = 1,
    date = c(as.Date('1989-12-13'), as.Date('1989-12-13'), as.Date('1989-12-20'))
  )
  ratings <- tibble(
    name = c('Peyton', 'Tom', 'Drew', 'Aaron'),
    mean = c(1700, 1300, 1500, 1500),
    variance = c(100, 100, 100, 100),
    date = as.Date('1989-12-12'),
    wins = c(100, 0, 50, 50),
    losses = c(0, 100, 50, 50)
  )
  season_result <- run_season(current_season, ratings)

  expect_is(season_result$ratings, 'tbl_df')
  expect_equal(
    season_result$ratings %>%
      filter(name == 'Peyton',
             date == max(date)) %>%
      .[['wins']],
    102)
  expect_gt(season_result$discrepancy, 0)
})
