
library(dplyr)
library(Rsolnp)

load("serie_a_2019.RData")

# Função de máxima verossimilhança

log_lik <- function(par) {
  # par = alpha_1, ... alpha_20,                        par[1:20]
  #       beta_1, ..., beta_20,                         par[21:40]
  #       gamma                                         par[41]
  #       csi_1, csi_2                                  par[42:43]
  #       ro_1, ro_2                                    par[44:45]
  
  # Funções auxiliares
  
  log_lik_k <- function(k) { # Verossimilhança para uma partida k
    
    lambda_xy <- function(t) {
      1
    }
    
    mu_xy <- function(t) {
      1
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
        s = s + integrate(function(t) lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = t1, upper = 44/90)$value # calcular a integral em (t1, 44]
        s = s + integrate(function(t) ro_1*lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 44/90, upper = 45/90)$value # calcular a integral em (44, 45]
        s = s + integrate(function(t) lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 45/90, upper = 89/90)$value # calcular a integral em (45, 89]
        s = s + integrate(function(t) ro_2*lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 89/90, upper = 90/90)$value # calcular a integral em (89, 90]
      } else if(check_ro_1) { # preciso dividir em 3 intervalos 
        s = s + integrate(function(t) lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = t1, upper = 44/90)$value # calcular a integral em (t1, 44]
        s = s + integrate(function(t) ro_1*lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 44/90, upper = 45/90)$value # calcular a integral em (44, 45]
        s = s + integrate(function(t) lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 45/90, upper = t2)$value # calcular a integral em (45, t2]
      } else if(check_ro_2) { # preciso dividir em 2 intervalos
        s = s + integrate(function(t) lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = t1, upper = 89/90)$value # calcular a integral em (t1, 89]
        s = s + integrate(function(t) ro_2*lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t, 
                          lower = 89/90, upper = 90/90)$value # calcular a integral em (89, 90]
      } else { # só preciso de 1 intervalo
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
        s = s + integrate(function(t) mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = t1, upper = 44/90)$value # calcular a integral em (t1, 44]
        s = s + integrate(function(t) ro_1*mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 44/90, upper = 45/90)$value # calcular a integral em (44, 45]
        s = s + integrate(function(t) mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 45/90, upper = 89/90)$value # calcular a integral em (45, 89]
        s = s + integrate(function(t) ro_2*mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 89/90, upper = 90/90)$value # calcular a integral em (89, 90]
      } else if(check_ro_1) { # preciso dividir em 3 intervalos
        s = s + integrate(function(t) mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = t1, upper = 44/90)$value # calcular a integral em (t1, 44]
        s = s + integrate(function(t) ro_1*mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 44/90, upper = 45/90)$value # calcular a integral em (44, 45]
        s = s + integrate(function(t) mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 45/90, upper = t2)$value # calcular a integral em (45, t2]
      } else if(check_ro_2) { # preciso dividir em 2 intervalos
        s = s + integrate(function(t) mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = t1, upper = 89/90)$value # calcular a integral em (t1, 89]
        s = s + integrate(function(t) ro_2*mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t, 
                          lower = 89/90, upper = 90/90)$value # calcular a integral em (89, 90]
      } else { # só preciso de 1 intervalo
        s = s + integrate(function(t) mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t, 
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
  
  alpha = par[1:20]
  beta = par[21:40]
  gamma = par[41]
  csi_1 = par[42]
  csi_2 = par[43]
  ro_1 = par[44]
  ro_2 = par[45]
  
  soma = 0
  print(par)
  for(k in 1:N) {
    soma = soma + log_lik_k(k)
  }
  -soma
}

# solnp

eqfun <- function(par) {
  sum(par[1:20])
}

set.seed(1)
sol = solnp(pars = rep(1, 45), log_lik, LB = rep(0, 45), eqfun = eqfun, eqB = n)

sol_modelo_3 = sol
save(sol_modelo_3, file = "sol/sol_modelo_3.RData")

