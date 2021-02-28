
load("2020/data/mod_2.RData")

pred_mod_2 <- function(n = 10000, home_team, away_team, score_home = 0, score_away = 0, minute = 0, half = 1) {
  
  pred <- function(home_team, away_team, score_home, score_away, minute, half) {
    
    if(half == 1) { # Tempo regulamentar do primeiro tempo
      while(minute < 45) {
        lambda = exp(alpha_i + beta_j + gamma)
        mu = exp(alpha_j + beta_i)
        next_home_goal = rexp(1, rate = lambda) + minute
        next_away_goal = rexp(1, rate = mu) + minute
        next_goal = min(next_home_goal, next_away_goal)
        if(next_goal < 45) {
          minute = next_goal
          if(next_home_goal == minute) {
            score_home = score_home + 1
          } else {
            score_away = score_away + 1
          }
        } else {
          minute = 45
        }
      }
      
      # Acréscimos do primeiro tempo
      U1 = rpois(1, lambda = mod_2$eta[1] + mod_2$phi[1]*(score_home + score_away))
      u1 = 0
      while(u1 < U1) {
        lambda = exp(alpha_i + beta_j + gamma)
        mu = exp(alpha_j + beta_i)
        next_home_goal = rexp(1, rate = lambda) + u1
        next_away_goal = rexp(1, rate = mu) + u1
        next_goal = min(next_home_goal, next_away_goal)
        if(next_goal < U1) {
          u1 = next_goal
          if(next_home_goal == u1) {
            score_home = score_home + 1
          } else {
            score_away = score_away + 1
          }
        } else {
          u1 = U1
          minute = 0
          half = 2
        }
      }
    }
    
    # Tempo regulamentar do segundo tempo
    while(minute < 45) {
      lambda = exp(alpha_i + beta_j + gamma + tau)
      mu = exp(alpha_j + beta_i + tau)
      next_home_goal = rexp(1, rate = lambda) + minute
      next_away_goal = rexp(1, rate = mu) + minute
      next_goal = min(next_home_goal, next_away_goal)
      if(next_goal < 45) {
        minute = next_goal
        if(next_home_goal == minute) {
          score_home = score_home + 1
        } else {
          score_away = score_away + 1
        }
      } else {
        minute = 45
      }
    }
    
    # Acréscimos do segundo tempo
    U2 = rpois(1, lambda = mod_2$eta[2] + mod_2$phi[2]*(score_home + score_away) +
                 mod_2$kappa * (abs(score_home - score_away) <= 1))
    u2 = 0
    while(u2 < U2) {
      lambda = exp(alpha_i + beta_j + gamma + tau)
      mu = exp(alpha_j + beta_i + tau)
      next_home_goal = rexp(1, rate = lambda) + u2
      next_away_goal = rexp(1, rate = mu) + u2
      next_goal = min(next_home_goal, next_away_goal)
      if(next_goal < U2) {
        u2 = next_goal
        if(next_home_goal == u2) {
          score_home = score_home + 1
        } else {
          score_away = score_away + 1
        }
      } else {
        u2 = U2
      }
    }
    
    c(score_home, score_away)
  }
  
  gamma = mod_2$gamma
  tau = mod_2$tau
  alpha_i = mod_2$alpha[home_team]
  beta_i = mod_2$beta[home_team]
  alpha_j = mod_2$alpha[away_team]
  beta_j = mod_2$beta[away_team]
  
  lst = list()
  for(i in 1:n) {
    lst[[i]] = pred(home_team, away_team, score_home, score_away, minute, half)
  }
  scores = do.call(rbind, lst)
  colnames(scores) = c(home_team, away_team)
  
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


