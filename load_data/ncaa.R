library(dplyr)
library(httr)
library(lubridate)
library(purrr)
library(readr)

# @ESPN - I'm just gonna use this until you say stop
# Don't worry, I'm not making money off of it
NCAA_SCOREBOARD <- 'http://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard'
SEASON_TYPES <- c(
  regular = 2,  # 15-16 weeks
  bowls = 3  # 1 week
)

.lazy_get <- function(url, query) {
  params <- paste(names(query), query, sep = '', collapse = '')
  file_name <- sprintf('load_data/the_internet/%s%s.rds',
                       gsub('[^[:alnum:] ]', '', url),
                       params)
  if (file.exists(file_name)) {
    readRDS(file_name)
  } else {
    content <- GET(NCAA_SCOREBOARD, query = query) %>% content()
    saveRDS(content, file_name)
    content
  }
}

.get_ncaa_scoreboard <- function(season, week, season_type) {
  .lazy_get(NCAA_SCOREBOARD, query = list(
    lang = 'en',
    region = 'us',
    calendartype = 'blacklist',
    limit = 300,
    dates = season,
    seasontype = season_type,
    week = week,
    groups = 80
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
    date = as.Date(event$date)
  )
}

.get_week_results <- function(season, week, season_type = SEASON_TYPES['regular']) {
  raw_week <- .get_ncaa_scoreboard(season, week, season_type)
  week_results <- raw_week$events %>%
    map(~.parse_game(.x)) %>%
    bind_rows()
  if (nrow(week_results) == 0) {
    # Empty weeks happen sometimes
    return(tibble())
  }
  # Set all the dates to the most common date in the week, most likely Saturday
  common_date <- week_results$date %>%
    table() %>%
    sort() %>%
    names() %>%
    last()
  week_results$date <- common_date
  week_results
}

.get_season_results <- function(season) {
  # Might only need to go to 16 for NCAA,
  # but weeks after the final week return empty lists anyway
  regular_season <- seq_len(17) %>%
    map(~.get_week_results(season, .x, SEASON_TYPES['regular'])) %>%
    bind_rows()
  post_season <- seq_len(5) %>%
    map(~.get_week_results(season, .x, SEASON_TYPES['post'])) %>%
    bind_rows()
  bind_rows(regular_season, post_season) %>%
    mutate(season)
}

.remove_spaces <- function(team_name) {
  # Some names look like Minnesota            Vikings
  gsub('\\s+', ' ', team_name)
}

seasons <- seq(1999, 2017)

# Lame: removing "cache" for current season
.clear_season <- function(season) {
  files <- dir('load_data/the_internet/', full.names = TRUE)
  season_files <- files[grepl(sprintf('dates%d', season), files)]
  file.remove(season_files)
}
.clear_season(seasons[length(seasons)])


# Yep, I just remove them for now...lame...
# Maybe when I get smarter, I can set them to a different "date"
.remove_duplicates <- function(day_games) {
  all_players <- c(day_games$winner, day_games$loser)
  is_duplicate <- duplicated(all_players) %>%
    matrix(byrow = F, ncol = 2) %>%
    apply(MARGIN = 1, FUN = any)
  day_games[!is_duplicate,]
}


ncaa_data <- seasons %>%
  map_df(~.get_season_results(.x)) %>%
  bind_rows() %>%
  mutate(group = 'ncaa') %>%
  split(.$date) %>%
  purrr::map_df(.remove_duplicates) %>%
  bind_rows() %>%
  write_csv('load_data/ncaa.csv')
