library(DT)
library(dplyr)
library(ggplot2)

function(input, output) {
  # Fill in the spot we created for a plot
  output$ratings <- DT::renderDataTable(
    ratings %>% filter_most_recent() %>%
      arrange(desc(mean))
  )

  # Okay, so ggvis+shiny+tooltip could use some work
  # This `eventReactive` + `reactive` pattern is what you use
  # when you're updating ggvis from something that's not a ggvis input
  # Gets weird b/c the way add_tooltip works w/ non-plotted columns
  # requires you to have a "key" column and access the DF by row
  # which means I have to double-access this `eventReactive`. Srry.
  my_ratings <- eventReactive(input$player_name, {
    comparisons %>% filter(player == input$player_name) %>%
      mutate(week_num = order(date_current)) %>%
      filter(!is.na(opponent)) %>%
      mutate(id = seq_len(n()))
  })
  reactive({
    my_ratings() %>%
      ggvis(x = ~week_num, y = ~mean_current, key := ~id) %>%
      layer_lines() %>%
      # layer_lines alone doesn't work w/ tooltips
      # so I also have to include points :(
      layer_points(size := 20) %>%
      add_tooltip(function(x) {
        # ggvis tooltip boilerplate for skipping null displays
        if(is.null(x)) {return(NULL)}
        row <- my_ratings()[x$id,]
        row$tooltip_text
      }, 'hover') %>%
      add_axis('x', title = 'Week') %>%
      add_axis('y', title = 'Rating')
  }) %>% bind_shiny('player')

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

    min_point <- min(ratings$mean) - 2.5 * sqrt(max(ratings$variance))
    max_point <- max(ratings$mean) + 2.5 * sqrt(max(ratings$variance))
    ggplot(data.frame(x = seq(min_point, max_point)), aes(x = x)) +
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
    rows <- comparisons %>% filter(date == input$selected_week) %>%
      select(
        player, opponent, result,
        rank_change, rank_current, rank_sign,
        player_score, opponent_score,
        mean_change_text, mean_current) %>%
      arrange(rank_current) %>%
      pmap(function(player, opponent, result,
                    rank_change, rank_current, rank_sign,
                    player_score, opponent_score,
                    mean_change_text, mean_current) {
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
      })
    tags$table(
      tableHeader(c('Rank', 'Rating', 'Player', 'Change', 'Result')),
      tags$tbody(rows),
      class = 'comparison-table'
    )
  })
}
