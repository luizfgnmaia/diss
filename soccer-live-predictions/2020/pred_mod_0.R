
load("2020/data/mod_0.RData")

pred_mod_0 <- function(n = 10000, home_team, away_team, score_home = 0, score_away = 0, minute = 0, half = 1, end_minute = 45, end_half = 2) {
  
  if(half == 1 & end_half == 1) {
    remaining_1st_half = end_minute - minute
  } else if(half == 1) { # 1 e 2
    remaining_1st_half = 45 - minute
  } else if(half == 2) { # 2 e 2
    remaining_1st_half = 0
  }
  
  if(half == 2 & end_half == 2) {
    remaining_2nd_half = end_minute - minute
  } else if(end_half == 2) { # 1 e 2
    remaining_2nd_half = end_minute
  } else if(end_half == 1) { # 1 e 1
    remaining_2nd_half = 0
  }
  
  remaining = remaining_1st_half + remaining_2nd_half
  
  gamma = mod_0$gamma
  alpha_i = mod_0$alpha[home_team]
  beta_i = mod_0$beta[home_team]
  alpha_j = mod_0$alpha[away_team]
  beta_j = mod_0$beta[away_team]
  
  home_goals = rpois(n, lambda = exp(alpha_i + beta_j + gamma) * remaining/90) + score_home
  away_goals = rpois(n, lambda = exp(alpha_j + beta_i) * remaining/90) + score_away
  scores = cbind(home_goals, away_goals)
  
  home_win = sum(scores[,1] > scores[,2])/n
  away_win = sum(scores[,1] < scores[,2])/n
  tie = 1 - home_win - away_win
  winner = c(home_win, tie, away_win)
  names(winner) = c(home_team, "Tie", away_team)
  
  freq_scores = head(sort(table(paste0(scores[,1], "-", scores[,2])), decreasing = TRUE), 10)/n
  freq_scores[11] = 1 - sum(freq_scores)
  names(freq_scores)[11] = "other"
  
  list("Result" = winner, "Score" = freq_scores)
}




