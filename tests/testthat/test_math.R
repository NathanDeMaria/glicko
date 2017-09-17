context("Math")

test_that("Update ratings updates", {
  new_rating <- update_ratings(1500, 100, 1500, 100, 1)
  expect_equal(new_rating$new_mean, 1500.287444)
  expect_equal(new_rating$new_variance, 99.91731)
})
