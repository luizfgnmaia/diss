
library(dplyr)
library(Rsolnp)

load("serie_a_14_19.RData")

# Função de máxima verossimilhança

log_lik <- function(par) {
  # par = alpha_1, ... alpha_20,                        par[1:31]
  #       beta_1, ..., beta_20,                         par[32:62]
  #       gamma                                         par[63]
  #       csi_1, csi_2                                  par[64:65]
  
  # Funções auxiliares
  
  log_lik_k <- function(k) { # Verossimilhança para uma partida k
    
    lambda_xy <- function(t) {
      1
    }
    
    mu_xy <- function(t) {
      1
    }
    
    lambda_k <- function(t) {
      lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t
    }
    
    mu_k <- function(t) {
      mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*t
    }
    
    int_lambda <- function(t1, t2) {
      integrate(Vectorize(lambda_k), lower = t1, upper = t2)$value # https://stackoverflow.com/questions/43818574/error-in-integrate-evaluation-of-function-gave-a-result-of-wrong-length
    }
    
    int_mu <- function(t1, t2) {
      integrate(Vectorize(mu_k), lower = t1, upper = t2)$value
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
sol = solnp(pars = rep(1, 65), log_lik, LB = rep(0, 65), eqfun = eqfun, eqB = n)

sol_modelo_2 = sol
save(sol_modelo_2, file = "sol/sol_modelo_2.RData")

