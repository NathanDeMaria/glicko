library(DT)
library(dplyr)
library(ggplot2)

function(input, output) {
  # Fill in the spot we created for a plot
  output$ratings <- DT::renderDataTable(
    ratings %>% filter_most_recent() %>%
      arrange(desc(mean))
  )

  output$player <- renderPlot({
    ratings %>% filter(name == input$player_name) %>%
      ggplot() + geom_line(aes(x = date, y = mean)) +
      geom_ribbon(aes(x = date,
                      ymin = mean - sqrt(variance),
                      ymax = mean + sqrt(variance)),
                  alpha = .2) +
      labs(title = input$player_name, x = 'Date', y = 'Rating')
  })

  output$player_table <- renderTable({
    ratings %>% filter_most_recent() %>%
      filter(name == input$player_name) %>%
      select(-date) %>% t() %>% data.frame()
  }, rownames = TRUE, colnames = FALSE)

  output$matchup <- renderPlot({
    p1 <- input$player_one
    p2 <- input$player_two
    # TODO: don't duplicate this from .filter_most_recent
    p1_rating <- ratings %>% filter(name == p1, date == max(date))
    p2_rating <- ratings %>% filter(name == p2, date == max(date))
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
      labs(title = winner_title) +
      theme(legend.title = element_blank())
  })

  output$comparison <- renderUI({
    comparisons[[as.integer(input$selected_week)]] %>%
      select(player, opponent, rank_change, rank_current, player_score, opponent_score, mean_change, mean_current) %>%
      arrange(rank_current) %>%
      pmap(function(player, opponent, rank_change, rank_current, player_score, opponent_score, mean_change, mean_current) {
        rank_sign <- ifelse(rank_change > 0, '↑',
                            ifelse(rank_change < 0, '↓', '↔'))
        result <- ifelse(player_score > opponent_score, 'win',
                         ifelse(player_score < opponent_score, 'loss', 'tie'))
        mean_change_text <- ifelse(mean_change > 0, sprintf('+%.01f', mean_change),
                                   ifelse(mean_change < 0, sprintf('-%.01f', abs(mean_change)), '↔'))
        htmlTemplate(
          'playerSummary.html',
          rank = rank_current,
          rank_sign = rank_sign,
          rank_change = abs(rank_change),
          player_name = player,
          win_loss = result,
          opponent = opponent,
          player_score = player_score,
          opponent_score = opponent_score,
          mean_change = mean_change_text,
          player_rating = sprintf('%.02f', mean_current),
          document_ = F
        )
    }) %>% div()
  })
}
