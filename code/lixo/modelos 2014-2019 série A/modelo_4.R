
library(dplyr)
library(Rsolnp)

load("serie_a_14_19.RData")

# Função de máxima verossimilhança

log_lik <- function(par) {
  # par = alpha_1, ... alpha_20,                        par[1:31]
  #       beta_1, ..., beta_20,                         par[32:62]
  #       gamma                                         par[63]
  #       csi_1, csi_2                                  par[64:65]
  #       ro_1, ro_2                                    par[66:67]
  #       lambda_10, lambda_01                          par[68:69]
  #       mu_10, mu_01                                  par[70:71]
  
  # Funções auxiliares
  
  log_lik_k <- function(k) { # Verossimilhança para uma partida k
    
    lambda_xy <- function(t) {
      x = lst_x[[k]][round(90*t+1, 0)]
      y = lst_y[[k]][round(90*t+1, 0)]
      ifelse(x-y==0, 1,
             ifelse(x-y>=1, lambda_10, lambda_01))
    }
    
    mu_xy <- function(t) {
      x = lst_x[[k]][round(90*t+1, 0)]
      y = lst_y[[k]][round(90*t+1, 0)]
      ifelse(x-y==0, 1,
             ifelse(x-y>=1, mu_10, mu_01))
    }
    
    lambda_k <- function(t) {
      ro_1^(t>44/90 & t<=45/90)*ro_2^(t>89/90)*lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t
    }
    
    mu_k <- function(t) {
      ro_1^(t>44/90 & t<=45/90)*ro_2^(t>89/90)*mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t
    }
    
    int_lambda <- function(t1, t2) {
      s = 0
      check_ro_1 = t1<44/90 & t2>=45/90
      check_ro_2 = t2>89/90
      if(check_ro_1 & check_ro_2) { # preciso dividir em 4 intervalos
        lambda_xy_1 = lambda_xy(44/90)
        lambda_xy_2 = lambda_xy(45/90)
        lambda_xy_3 = lambda_xy(89/90)
        lambda_xy_4 = lambda_xy(90/90)
        s = s + integrate(function(t) lambda_xy_1*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = t1, upper = 44/90)$value # calcular a integral em (t1, 44]
        s = s + integrate(function(t) ro_1*lambda_xy_2*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 44/90, upper = 45/90)$value # calcular a integral em (44, 45]
        s = s + integrate(function(t) lambda_xy_3*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 45/90, upper = 89/90)$value # calcular a integral em (45, 89]
        s = s + integrate(function(t) ro_2*lambda_xy_4*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 89/90, upper = 90/90)$value # calcular a integral em (89, 90]
      } else if(check_ro_1) { # preciso dividir em 3 intervalos 
        lambda_xy_1 = lambda_xy(44/90)
        lambda_xy_2 = lambda_xy(45/90)
        lambda_xy_3 = lambda_xy(t2)
        s = s + integrate(function(t) lambda_xy_1*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = t1, upper = 44/90)$value # calcular a integral em (t1, 44]
        s = s + integrate(function(t) ro_1*lambda_xy_2*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 44/90, upper = 45/90)$value # calcular a integral em (44, 45]
        s = s + integrate(function(t) lambda_xy_3*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 45/90, upper = t2)$value # calcular a integral em (45, t2]
      } else if(check_ro_2) { # preciso dividir em 2 intervalos
        lambda_xy_1 = lambda_xy(89/90)
        lambda_xy_2 = lambda_xy(90/90)
        s = s + integrate(function(t) lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = t1, upper = 89/90)$value # calcular a integral em (t1, 89]
        s = s + integrate(function(t) ro_2*lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 89/90, upper = 90/90)$value # calcular a integral em (89, 90]
      } else { # só preciso de 1 intervalo
        lambda_xy_1 = lambda_xy(t2)
        s = s + integrate(function(t) lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = t1, upper = t2)$value # calcular a integral em (t1, t2]
      }
      s 
    }
    
    int_mu <- function(t1, t2) {
      s = 0
      check_ro_1 = t1<44/90 & t2>=45/90
      check_ro_2 = t2>89/90
      if(check_ro_1 & check_ro_2) { # preciso dividir em 4 intervalos
        mu_xy_1 = mu_xy(44/90)
        mu_xy_2 = mu_xy(45/90)
        mu_xy_3 = mu_xy(89/90)
        mu_xy_4 = mu_xy(90/90)
        s = s + integrate(function(t) mu_xy_1*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = t1, upper = 44/90)$value # calcular a integral em (t1, 44]
        s = s + integrate(function(t) ro_1*mu_xy_2*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 44/90, upper = 45/90)$value # calcular a integral em (44, 45]
        s = s + integrate(function(t) mu_xy_3*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 45/90, upper = 89/90)$value # calcular a integral em (45, 89]
        s = s + integrate(function(t) ro_2*mu_xy_4*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 89/90, upper = 90/90)$value # calcular a integral em (89, 90]
      } else if(check_ro_1) { # preciso dividir em 3 intervalos
        mu_xy_1 = mu_xy(44/90)
        mu_xy_2 = mu_xy(45/90)
        mu_xy_3 = mu_xy(t2)
        s = s + integrate(function(t) mu_xy_1*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = t1, upper = 44/90)$value # calcular a integral em (t1, 44]
        s = s + integrate(function(t) ro_1*mu_xy_2*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 44/90, upper = 45/90)$value # calcular a integral em (44, 45]
        s = s + integrate(function(t) mu_xy_3*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 45/90, upper = t2)$value # calcular a integral em (45, t2]
      } else if(check_ro_2) { # preciso dividir em 2 intervalos
        mu_xy_1 = mu_xy(89/90)
        mu_xy_2 = mu_xy(90/90)
        s = s + integrate(function(t) mu_xy_1*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = t1, upper = 89/90)$value # calcular a integral em (t1, 89]
        s = s + integrate(function(t) ro_2*mu_xy_2*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 89/90, upper = 90/90)$value # calcular a integral em (89, 90]
      } else { # só preciso de 1 intervalo
        mu_xy_1 = mu_xy(t2)
        s = s + integrate(function(t) mu_xy_1*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = t1, upper = t2)$value # calcular a integral em (t1, t2]
      }
      s 
    }
    
    xy = paste0(lst_x[[k]], "-", lst_y[[k]])
    tab = table(xy)
    if(length(tab) > 1) {
      acum = c(0, cumsum(tab)-1)
      inicio = acum[-length(acum)]
      fim = acum[-1]
    } else {
      inicio = 0
      fim = 90
    }
    soma_int_lambda = 0
    soma_int_mu = 0
    for(int in 1:length(inicio)) {
      soma_int_lambda = soma_int_lambda + int_lambda(t1 = inicio[int]/90, t2 = fim[int]/90)
      soma_int_mu = soma_int_mu + int_mu(t1 = inicio[int]/90, t2 = fim[int]/90)
    }
    mk = ifelse(is.na(lst_J[[k]][1]), 0, length(lst_J[[k]]))
    if(mk > 0) {
      soma_l_mk = sum(log(lambda_k(lst_t[[k]])^(1-lst_J[[k]])*mu_k(lst_t[[k]])^(lst_J[[k]])))
    } else {
      soma_l_mk = 0
    }
    soma_l_mk - soma_int_lambda - soma_int_mu
  }
  
  alpha = par[1:31]
  beta = par[32:62]
  gamma = par[63]
  csi_1 = par[64]
  csi_2 = par[65]
  ro_1 = par[66]
  ro_2 = par[67]
  lambda_10 = par[68]
  lambda_01 = par[69]
  mu_10 = par[70]
  mu_01 = par[71]
  
  soma = 0
  print(par)
  for(k in 1:N) {
    soma = soma + log_lik_k(k)
  }
  -soma
}

# solnp

eqfun <- function(par) {
  sum(par[1:31])
}

set.seed(1)
#sol = solnp(pars = rep(1, 71), log_lik, LB = rep(0, 71), eqfun = eqfun, eqB = n)
#sol_modelo_4 = sol
#save(sol_modelo_4, file = "sol/sol_modelo_4.RData")

load("sol/sol_modelo_3.RData")
sol = solnp(pars = c(sol_modelo_3$pars, 1, 1, 1, 1), log_lik, LB = rep(0, 71), eqfun = eqfun, eqB = n)
sol_modelo_4b = sol
save(sol_modelo_4b, file = "sol/sol_modelo_4b.RData")

