
load("mod_0.RData")

pred_mod_0 <- function(n = 10000, home_team, away_team) {
  
  gamma = mod_0$gamma
  alpha_i = mod_0$alpha[home_team]
  beta_i = mod_0$beta[home_team]
  alpha_j = mod_0$alpha[away_team]
  beta_j = mod_0$beta[away_team]
  
  home_goals = rpois(n, lambda = exp(alpha_i + beta_j + gamma))
  away_goals = rpois(n, lambda = exp(alpha_j + beta_i))
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
    




