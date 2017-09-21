library(dplyr)
library(httr)
library(purrr)
library(readr)

# @ESPN - I'm just gonna use this until you say stop
# Don't worry, I'm not making money off of it
NFL_SCOREBOARD <- 'http://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard'
SEASON_TYPES <- c(regular = 2, post = 3)

.lazy_get <- function(url, query) {
  params <- paste(names(query), query, sep = '', collapse = '')
  file_name <- sprintf('load_data/the_internet/%s%s.rds',
                       gsub('[^[:alnum:] ]', '', url),
                       params)
  if (file.exists(file_name)) {
    readRDS(file_name)
  } else {
    content <- GET(NFL_SCOREBOARD, query = query) %>% content()
    saveRDS(content, file_name)
    content
  }
}

.get_nfl_scoreboard <- function(season, week, season_type) {
  .lazy_get(NFL_SCOREBOARD, query = list(
    lang = 'en',
    region = 'us',
    calendartype = 'blacklist',
    limit = 100,
    dates = season,
    seasontype = season_type,
    week = week
  ))
}

.parse_game <- function(event) {
  # event from the ESPN api
  if (!event$status$type$completed) {
    return(tibble())
  }

  teams <- event$competitions[[1]]$competitors
  team_names <- teams %>% map_chr(~.x$team$displayName)
  scores <- teams %>% map_chr(~.x$score) %>% as.integer()
  is_winner <- teams %>% map_lgl(~.x$winner)

  if (!any(is_winner)) {
    # If tie, "winner" is the first team
    # Doesn't matter b/c the scores are what's used from this
    is_winner <- c(TRUE, FALSE)
  }

  tibble(
    winner = team_names[is_winner],
    loser = team_names[!is_winner],
    winner_score = scores[is_winner],
    loser_score = scores[!is_winner],
    # TODO: care about specific time? or group this by date?
    date = as.Date(event$date)
  )
}

.get_week_results <- function(season, week, season_type = SEASON_TYPES['regular']) {
  raw_week <- .get_nfl_scoreboard(season, week, season_type)
  week_results <- raw_week$events %>%
    map(~.parse_game(.x)) %>%
    bind_rows()
}

.get_season_results <- function(season) {
  regular_season <- seq_len(17) %>%
    map(~.get_week_results(season, .x, SEASON_TYPES['regular'])) %>%
    bind_rows()
  post_season <- seq_len(5) %>%
    map(~.get_week_results(season, .x, SEASON_TYPES['post'])) %>%
    bind_rows() %>%
    # NFC/AFC/Team Rice, other pro bowls
    filter(!grepl('[NA]FC', winner)) %>%
    filter(!grepl('Team', winner))
  bind_rows(regular_season, post_season) %>%
    mutate(season)
}

.remove_spaces <- function(team_name) {
  # Some names look like Minnesota            Vikings
  gsub('\\s+', ' ', team_name)
}

.replace_old_names <- function(team_name) {
  recode(team_name, 'St. Louis Rams' = 'Los Angeles Rams') %>%
    recode('San Diego Chargers' = 'Los Angeles Chargers')
}

seasons <- seq(1999, 2017)

# Lame: removing "cache" for current season
.clear_season <- function(season) {
  files <- dir('load_data/the_internet/', full.names = TRUE)
  season_files <- files[grepl(sprintf('dates%d', season), files)]
  file.remove(season_files)
}

.clear_season(seasons[length(seasons)])

nfl_data <- seasons %>%
  map_df(~.get_season_results(.x)) %>%
  bind_rows() %>%
  mutate(winner = .remove_spaces(winner) %>% .replace_old_names()) %>%
  mutate(loser = .remove_spaces(loser) %>% .replace_old_names()) %>%
  # TODO: divisions? doesn't matter as much
  mutate(group = 'nfl') %>%
  write_csv('load_data/nfl.csv')
