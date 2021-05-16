
input_value <- function(home_team, away_team, date) {
  
  res = results %>%
    filter(Date == date,
           Home_Team == home_team,
           Away_Team == away_team)
  
  list(Value_Home = res$Value_Home, Value_Away = res$Value_Away)
}


