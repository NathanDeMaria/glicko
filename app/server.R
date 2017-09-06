library(DT)
library(dplyr)
library(ggplot2)

function(input, output) {
  # Fill in the spot we created for a plot
  output$ratings <- DT::renderDataTable(
    ratings %>% filter(week == max(week)) %>%
      arrange(desc(mean))
  )

  output$player <- renderPlot({
    ratings %>% filter(name == input$player_name) %>%
      ggplot() + geom_line(aes(x = week, y = mean)) +
      geom_ribbon(aes(x = week,
                      ymin = mean - sqrt(variance),
                      ymax = mean + sqrt(variance)),
                  alpha = .2) +
      labs(title = input$player_name, x = 'Week', y = 'Rating')
  })

  output$matchup <- renderPlot({
    p1 <- input$player_one
    p2 <- input$player_two
    # TODO: don't duplicate this from .filter_most_recent
    p1_rating <- ratings %>% filter(name == p1, week == max(week))
    p2_rating <- ratings %>% filter(name == p2, week == max(week))
    win_prob <- matchup(ratings, p1, p2)

    winner_title <- if (win_prob > .5) {
      sprintf("%s %.02f%% over %s", p1, win_prob * 100, p2)
    } else {
      sprintf("%s %.02f%% over %s", p2, 100 - win_prob * 100, p1)
    }

    ggplot(data.frame(x = seq(0, 3000)), aes(x = x)) +
      stat_function(fun = dnorm, n = 3000,
                    args = list(mean = p1_rating$mean, sd = sqrt(p1_rating$variance)),
                    aes(color = 'Player 1')) +
      stat_function(fun = dnorm, n = 3000,
                    args = list(mean = p2_rating$mean, sd = sqrt(p2_rating$variance)),
                    aes(color = 'Player 2')) +
      labs(title = winner_title)
  })
}
