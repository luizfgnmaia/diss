
pred_mod_9 <- function(mod_9, n = 10^5, home_team, away_team, score_home = 0, score_away = 0, reds_home_1 = 0, reds_away_1 = 0, reds_home_2 = 0, reds_away_2 = 0, minute = 0, half = 1, end_minute = 45, end_half = 2, stoppage_time = TRUE, value_home, value_away) {
  
  if(!is.numeric(n) | n <= 0) {
    stop("Invalid n.")
  }
  
  n = as.integer(ceiling(n))
  
  if(!home_team %in% names(mod_9$alpha)) {
    stop("Invalid home team.")
  }
  
  if(!away_team %in% names(mod_9$alpha)) {
    stop("Invalid away team.")
  }
  
  if(!is.numeric(score_home) | score_home < 0) {
    stop("Invalid score_home.")
  }
  
  if(!is.numeric(score_away) | score_away < 0) {
    stop("Invalid score_away.")
  }
  
  if(!is.numeric(reds_home_1) | reds_home_1 < 0) {
    stop("Invalid reds_home_1.")
  }
  
  if(!is.numeric(reds_away_1) | reds_away_1 < 0) {
    stop("Invalid reds_away_1.")
  }
  
  if(!is.numeric(reds_home_2) | reds_home_2 < 0) {
    stop("Invalid reds_home_1.")
  }
  
  if(!is.numeric(reds_away_2) | reds_away_2 < 0) {
    stop("Invalid reds_away_2.")
  }
  
  if(minute > 45 | minute < 0) {
    stop("Invalid minute.")
  }
  
  if(!half %in% c(1,2)) {
    stop("Invalid half.")
  }
  
  if((end_minute > 45 | end_minute < 0)) {
    stop("Invalid end_minute.")
  }
  
  if(!end_half %in% c(1,2)) {
    stop("Invalid end_half.")
  }
  
  if(!is.logical(stoppage_time)) {
    stop("stoppage_time must be boolean.")
  }
  
  if(end_half < half) {
    stop("end_half can't be smaller than half.")
  } else if(end_half == half) {
    if(end_minute < minute) {
      stop("end_minute needs to be bigger than minute.")
    } else if(end_minute == minute & stoppage_time == FALSE) {
      stop("end_minute needs to be bigger than minute or stoppage_time needs to be TRUE.")
    }
  }
  
  pred <- function(home_team, away_team, score_home, score_away, reds_home_1, reds_away_1, reds_home_2, reds_away_2, minute, half, end_minute, end_half, stoppage_time, value_home, value_away) {
    
    reds_home = reds_home_1 + reds_home_2
    reds_away = reds_away_1 + reds_away_2
    
    dif_value = (value_home - value_away)/10^6
    
    if(end_minute < 45) {
      stoppage_time = FALSE
    }
    
    if(end_half == 1) {
      half = 1
    }
    
    if(half == 2) {
      end_half = 2
    }
    
    # Passo 1: tempo regulamentar do primeiro tempo
    if(half == 1) {
      
      if(end_half == 1) {
        end_1st = end_minute
      } else {
        end_1st = 45
      }
      
      # Gerando expulsões para o time mandante
      t = minute
      s = (1/2) * A_lambda * t^2 # começando do minuto t
      t_reds_home = NULL
      while(t < end_1st) { 
        u = runif(1)
        s = s - log(u)
        t = inv_lambda(s)
        if(t < end_1st) {
          t_reds_home = c(t_reds_home, t)
        }
      }
      
      # Gerando expulsões para o time visitante
      t = minute
      s = (1/2) * A_mu * t^2 # começando do minuto t
      t_reds_away = NULL
      while(t < end_1st) { 
        u = runif(1)
        s = s - log(u)
        t = inv_mu(s)
        if(t < end_1st) {
          t_reds_away = c(t_reds_away, t)
        }
      }
      
      # Gerando gols para ambos os times
      while(minute < end_1st) {
        
        ind_xy = xy(score_home, score_away) 
        lambda = exp(alpha_i + beta_j + gamma + lambda_xy[ind_xy] + omega_red*(reds_away-reds_home) + omega_value*(dif_value))
        mu = exp(alpha_j + beta_i + mu_xy[ind_xy] + omega_red*(reds_home-reds_away) - omega_value*(dif_value))
        next_home_goal = rexp(1, rate = lambda) + minute
        next_away_goal = rexp(1, rate = mu) + minute
        next_home_red = t_reds_home[t_reds_home > minute][1]
        if(length(next_home_red) == 0) {
          next_home_red = Inf
        } else if(is.na(next_home_red)) {
          next_home_red = Inf
        }
        next_away_red = t_reds_away[t_reds_away > minute][1]
        next_event = min(next_home_goal, next_away_goal, next_home_red, next_away_red, na.rm = TRUE)
        
        if(next_event < end_1st) {
          minute = next_event
          if(next_home_goal == minute) {
            score_home = score_home + 1
          } else if(next_away_goal == minute) {
            score_away = score_away + 1
          } else if(next_home_red == minute) {
            reds_home = reds_home + 1
          } else {
            reds_away = reds_away + 1
          }
        } else {
          minute = end_1st
        }
      }
      
      # Passo 2: acréscimos do primeiro tempo
      if(stoppage_time == TRUE | end_half == 2) {
        
        U1 = rpois(1, lambda = eta[1] + rho[1]*(reds_home + reds_away))
        
        # Gerando expulsões para o time mandante
        t = 45
        s = (1/2) * A_lambda * t^2 # começando do minuto 45
        t_reds_home_st = NULL
        while(t < 45 + U1) {
          u = runif(1)
          s = s - log(u)
          t = inv_lambda(s)
          if(t < 45) {
            t_reds_home_st = c(t_reds_home_st, t)
          }
        }
        
        # Gerando expulsões para o time visitante
        t = 45
        s = (1/2) * A_mu * t^2 # começando do minuto 45
        t_reds_away_st = NULL
        while(t < 45 + U1) {
          u = runif(1)
          s = s - log(u)
          t = inv_mu(s)
          if(t < 45 + U1) {
            t_reds_away_st = c(t_reds_away, t)
          }
        }
        
        # Gerando gols para ambos os times
        while(minute < 45 + U1) {
          
          ind_xy = xy(score_home, score_away)
          lambda = exp(alpha_i + beta_j + gamma + lambda_xy[ind_xy] + omega_red*(reds_away-reds_home) + omega_value*(dif_value))
          mu = exp(alpha_j + beta_i + mu_xy[ind_xy] + omega_red*(reds_home-reds_away) - omega_value*(dif_value))
          next_home_goal = rexp(1, rate = lambda) + minute
          next_away_goal = rexp(1, rate = mu) + minute
          next_home_red = t_reds_home[t_reds_home > minute][1]
          if(length(next_home_red) == 0) {
            next_home_red = Inf
          } else if(is.na(next_home_red)) {
            next_home_red = Inf
          }
          next_away_red = t_reds_away[t_reds_away > minute][1]
          next_event = min(next_home_goal, next_away_goal, next_home_red, next_away_red, na.rm = TRUE)
          
          if(next_event < 45 + U1) {
            minute = next_event
            if(next_home_goal == minute) {
              score_home = score_home + 1
            } else if(next_away_goal == minute) {
              score_away = score_away + 1
            } else if(next_home_red == minute) {
              reds_home = reds_home + 1
            } else {
              reds_away = reds_away + 1
            }
          } else {
            minute = 45 + U1
            half = 2
          }
        }
        minute = 0
        reds_home_1 = reds_home
        reds_away_1 = reds_away
      }
    }
    
    # Passo 3: tempo regulamentar do segundo tempo
    if(end_half == 2) {
      
      end_2nd = end_minute
      
      # Gerando expulsões para o time mandante
      t = minute + 45 
      s = (1/2) * A_lambda * t^2 # começando do minuto t
      t_reds_home = NULL
      while(t < end_2nd + 45) { # antes de calcular os acréscimos
        u = runif(1)
        s = s - log(u)
        t = inv_lambda(s)
        if(t < end_2nd + 45) {
          t_reds_home = c(t_reds_home, t - 45)
        }
      }
      
      # Gerando expulsões para o time visitante
      t = minute + 45
      s = (1/2) * A_mu * t^2 # começando do minuto t
      t_reds_away = NULL
      while(t < end_2nd + 45) { # antes de calcular os acréscimos
        u = runif(1)
        s = s - log(u)
        t = inv_mu(s)
        if(t < end_2nd + 45) {
          t_reds_away = c(t_reds_away, t - 45)
        }
      }
      
      # Gerando gols para ambos os times
      while(minute < end_2nd) {
        
        ind_xy = xy(score_home, score_away)
        lambda = exp(alpha_i + beta_j + gamma + tau + lambda_xy[ind_xy] + omega_red*(reds_away-reds_home) + omega_value*(dif_value))
        mu = exp(alpha_j + beta_i + tau + mu_xy[ind_xy] + omega_red*(reds_home-reds_away) - omega_value*(dif_value))
        next_home_goal = rexp(1, rate = lambda) + minute
        next_away_goal = rexp(1, rate = mu) + minute
        next_home_red = t_reds_home[t_reds_home > minute][1]
        if(length(next_home_red) == 0) {
          next_home_red = Inf
        } else if(is.na(next_home_red)) {
          next_home_red = Inf
        }
        next_away_red = t_reds_away[t_reds_away > minute][1]
        next_event = min(next_home_goal, next_away_goal, next_home_red, next_away_red, na.rm = TRUE)
        
        if(next_event < end_2nd) {
          minute = next_event
          if(next_home_goal == minute) {
            score_home = score_home + 1
          } else if(next_away_goal == minute) {
            score_away = score_away + 1
          } else if(next_home_red == minute) {
            reds_home = reds_home + 1
          } else {
            reds_away = reds_away + 1
          }
        } else {
          minute = end_2nd
        }
      }
      
      # Passo 4: acréscimos do segundo
      if(stoppage_time == TRUE) {
        
        U2 = rpois(1, lambda = eta[2] + rho[2]*(reds_home + reds_away - reds_home_1 - reds_away_1))
        
        # Gerando expulsões para o time mandante
        t = 90
        s = (1/2) * A_lambda * t^2 # começando do minuto 90
        t_reds_home_st = NULL
        while(t < 90 + U2) {
          u = runif(1)
          s = s - log(u)
          t = inv_lambda(s)
          if(t < 90 + U2) {
            t_reds_home_st = c(t_reds_home_st, t - 45)
          }
        }
        
        # Gerando expulsões para o time visitante
        t = 90
        s = (1/2) * A_mu * t^2 # começando do minuto 90
        t_reds_away_st = NULL
        while(t < 90 + U2) {
          u = runif(1)
          s = s - log(u)
          t = inv_mu(s)
          if(t < 90 + U2) {
            t_reds_away_st = c(t_reds_away, t - 45)
          }
        }
        
        # Gerando gols para ambos os times
        while(minute < 45 + U2) {
          
          ind_xy = xy(score_home, score_away)
          lambda = exp(alpha_i + beta_j + gamma + tau + lambda_xy[ind_xy] + omega_red*(reds_away-reds_home) + omega_value*(dif_value))
          mu = exp(alpha_j + beta_i + tau + mu_xy[ind_xy] + omega_red*(reds_home-reds_away) - omega_value*(dif_value))
          next_home_goal = rexp(1, rate = lambda) + minute
          next_away_goal = rexp(1, rate = mu) + minute
          next_home_red = t_reds_home[t_reds_home > minute][1]
          if(length(next_home_red) == 0) {
            next_home_red = Inf
          } else if(is.na(next_home_red)) {
            next_home_red = Inf
          }
          next_away_red = t_reds_away[t_reds_away > minute][1]
          next_event = min(next_home_goal, next_away_goal, next_home_red, next_away_red, na.rm = TRUE)
          
          if(next_event < 45 + U2) {
            minute = next_event
            if(next_home_goal == minute) {
              score_home = score_home + 1
            } else if(next_away_goal == minute) {
              score_away = score_away + 1
            } else if(next_home_red == minute) {
              reds_home = reds_home + 1
            } else {
              reds_away = reds_away + 1
            }
          } else {
            minute = 45 + U2
          }
        }
      }
    }
    c(score_home, score_away)
  }
  
  inv_lambda <- function(x) {
    (2*x/A_lambda)^(1/2)
  }
  
  inv_mu <- function(x) {
    (2*x/A_mu)^(1/2)
  }
  
  xy <- function(x, y) {
    if(x == y) {
      return(1)
    } else if(x >= y) {
      return(2)
    } else if(x <= y) {
      return(3)
    } else {
      raise("Error")
    }
  }
  
  alpha_i = mod_9$alpha[home_team]
  beta_i = mod_9$beta[home_team]
  alpha_j = mod_9$alpha[away_team]
  beta_j = mod_9$beta[away_team]
  gamma = mod_9$gamma
  tau = mod_9$tau
  lambda_xy = c(0, mod_9$omega_xy)
  mu_xy = c(0, rev(mod_9$omega_xy))
  omega_red = mod_9$omega["red"]
  omega_value = mod_9$omega["value"]
  A_lambda = exp(mod_9$a[1])
  A_mu = exp(mod_9$a[2])
  eta = mod_9$eta
  rho = mod_9$rho
  
  if(rho[1] < 0) {
    rho[1] = 0
  }
  
  if(rho[2] < 0) {
    rho[2] = 0
  }
  
  lst = future_lapply(1:n, function(x) pred(home_team, away_team, score_home, score_away, reds_home_1, reds_away_1, reds_home_2, reds_away_2, minute, half, end_minute, end_half, stoppage_time, value_home, value_away),
                      future.seed = TRUE)
  scores = do.call(rbind, lst)
  colnames(scores) = c(home_team, away_team)
  
  home_win = sum(scores[,1] > scores[,2])/n
  away_win = sum(scores[,1] < scores[,2])/n
  draw = 1 - home_win - away_win
  winner = c(home_win, draw, away_win)
  names(winner) = c(home_team, "Draw", away_team)
  freq_scores = sort(table(paste0(scores[,1], "-", scores[,2])), decreasing = TRUE)/n
  
  list("Result" = winner, "Score" = freq_scores)
}


