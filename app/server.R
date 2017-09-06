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
}
