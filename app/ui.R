players <- ratings %>% dplyr::select(name) %>% unique()

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
    plotOutput('matchup'))
)