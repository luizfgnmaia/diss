
pred_mod_0 <- function(mod_0, home_team, away_team, score_home = 0, score_away = 0, minute = 0, half = 1, end_minute = 45, end_half = 2) {
  
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
  
  tmp_hg = NULL
  tmp_ag = NULL
  home_goals = NULL
  away_goals = NULL
  for(g in 1:21) {
    tmp_hg[g] = dpois(g-1, lambda = exp(alpha_i + beta_j + gamma) * remaining/90)
    tmp_ag[g] = dpois(g-1, lambda = exp(alpha_j + beta_i) * remaining/90) 
  }
  for(g in (score_home+1):21) {
    home_goals[g] = tmp_hg[g-score_home]
  }
  home_goals[which(is.na(home_goals))] = 0
  for(g in (score_away+1):21) {
    away_goals[g] = tmp_ag[g-score_away]
  }
  away_goals[which(is.na(away_goals))] = 0
  
  mat_results = home_goals %*% t(away_goals)
  rownames(mat_results) = 0:20
  colnames(mat_results) = 0:20
  
  home_win = sum(mat_results[lower.tri(mat_results)])
  away_win = sum(mat_results[upper.tri(mat_results)])
  draw = 1 - home_win - away_win
  winner = c(home_win, draw, away_win)
  names(winner) = c(home_team, "Draw", away_team)
  
  freq_scores = NULL
  nam = NULL
  for(i in 1:21) {
    for(j in 1:21) {
      freq_scores = c(freq_scores, mat_results[i,j])
      nam = c(nam, paste0(rownames(mat_results)[i], "-", rownames(mat_results)[j])) 
    }
  }
  names(freq_scores) = nam
  freq_scores = sort(freq_scores, decreasing = TRUE)
  
  list("Result" = winner, "Score" = freq_scores)
}


