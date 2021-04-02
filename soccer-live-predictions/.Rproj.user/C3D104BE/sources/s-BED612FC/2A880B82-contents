
load("scrape/data/goals.RData")
load("scrape/data/reds.RData")
load("2020/data/input.RData")

input_pred <- function(ind, min) {
  
  goals$Minute[which(goals$Half == 2)] = goals$Minute[which(goals$Half == 2)] + 45
  reds$Minute[which(reds$Half == 2)] = reds$Minute[which(reds$Half == 2)] + 45
  
  match_goals = goals %>%
    filter(Season == copy_results$Season[ind],
           Match == copy_results$Match[ind])
  
  home_goals = match_goals %>%
    filter(Team == 1,
           Minute <= min, 
           is.na(Stoppage_Time)) %>%
    nrow()
  
  away_goals = match_goals %>%
    filter(Team == 2,
           Minute <= min, 
           is.na(Stoppage_Time)) %>%
    nrow()
  
  match_reds = reds %>%
    filter(Season == copy_results$Season[ind],
           Match == copy_results$Match[ind])
  
  home_reds_1 = match_reds %>%
    filter(Team == 1,
           Half == 1,
           Minute <= min, 
           is.na(Stoppage_Time)) %>%
    nrow()
  
  away_reds_1 = match_reds %>%
    filter(Team == 2,
           Half == 1,
           Minute <= min, 
           is.na(Stoppage_Time)) %>%
    nrow()
  
  home_reds_2 = match_reds %>%
    filter(Team == 1,
           Half == 2,
           Minute <= min, 
           is.na(Stoppage_Time)) %>%
    nrow()
  
  away_reds_2 = match_reds %>%
    filter(Team == 2,
           Half == 2,
           Minute <= min, 
           is.na(Stoppage_Time)) %>%
    nrow()
  
  
  list(home_goals = home_goals, away_goals = away_goals,
       home_reds_1 = home_reds_1, away_reds_1 = away_reds_1,
       home_reds_2 = home_reds_2, away_reds_2 = away_reds_2)
}


