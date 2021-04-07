
input_pred <- function(ind, min) {
  
  goals$Minute[which(goals$Half == 2)] = goals$Minute[which(goals$Half == 2)] + 45
  reds$Minute[which(reds$Half == 2)] = reds$Minute[which(reds$Half == 2)] + 45
  
  match_goals = goals %>%
    filter(Season == copy_results$Season[ind],
           Match == copy_results$Match[ind])
  
  score_home = match_goals %>%
    filter(Team == 1,
           Minute <= min) %>%
    nrow()
  
  score_away = match_goals %>%
    filter(Team == 2,
           Minute <= min) %>%
    nrow()
  
  match_reds = reds %>%
    filter(Season == copy_results$Season[ind],
           Match == copy_results$Match[ind])
  
  reds_home_1 = match_reds %>%
    filter(Team == 1,
           Half == 1,
           Minute <= min) %>%
    nrow()
  
  reds_away_1 = match_reds %>%
    filter(Team == 2,
           Half == 1,
           Minute <= min) %>%
    nrow()
  
  reds_home_2 = match_reds %>%
    filter(Team == 1,
           Half == 2,
           Minute <= min) %>%
    nrow()
  
  reds_away_2 = match_reds %>%
    filter(Team == 2,
           Half == 2,
           Minute <= min) %>%
    nrow()
  
  
  list(score_home = score_home, score_away = score_away,
       reds_home_1 = reds_home_1, reds_away_1 = reds_away_1,
       reds_home_2 = reds_home_2, reds_away_2 = reds_away_2)
}


