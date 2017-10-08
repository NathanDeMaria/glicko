players <- ratings %>% dplyr::select(name) %>% unique()
dates <- match_results$date %>% unique() %>% sort(decreasing = T)
date_choices <- seq_along(dates) %>% rev()
names(date_choices) <- dates

navbarPage("ELO",
  tabPanel("Ratings", DT::dataTableOutput('ratings')),
  tabPanel("Players",
    selectInput(
      'player_name', label = 'Player',
      choices = players),
    plotOutput('player'),
    tableOutput('player_table')),
  tabPanel("Matchups",
    selectInput(
      'player_one', label = 'Player 1', choices = players
    ),
    selectInput(
      'player_two', label = 'Player 2', choices = players
    ),
    plotOutput('matchup')),
  tabPanel("Weekly Updates",
    selectInput('selected_week', label = 'Week', choices = date_choices),
    htmlOutput('comparison')),
  theme = 'table.css'
)
