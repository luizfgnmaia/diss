
library(Rsolnp)

load("serie_a_2019.RData")

log_lik <- function(par) {

  log_lik_k <- function(k) { 
    
    lambda_xy <- function(t) {
      1
    }
    
    mu_xy <- function(t) {
      1
    }
    
    lambda_k <- function(t) {
      lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]
    }
    
    mu_k <- function(t) {
      mu_xy(t)*alpha[j[k]]*beta[i[k]]
    }
    
    int_lambda <- function(t1, t2) { 
      lambda_xy(t1)*gamma*alpha[i[k]]*beta[j[k]] * (t2-t1)
    }
    
    int_mu <- function(t1, t2) { 
      mu_xy(t1)*alpha[j[k]]*beta[i[k]] * (t2-t1)
    }
    
    v_int_lambda = NULL 
    v_int_mu = NULL 
    for(int in 1:(length(lst_int[[k]])-1)) {
      v_int_lambda[int] = int_lambda(t1 = lst_int[[k]][int]/90, t2 = lst_int[[k]][int+1]/90)
      v_int_mu[int] = int_mu(t1 = lst_int[[k]][int]/90, t2 = lst_int[[k]][int+1]/90)
    }
    
    sum_l_mk = ifelse(is.na(lst_J[[k]][1]), 0, 
                      sum((1-lst_J[[k]])*log(lambda_k(lst_t[[k]]))+lst_J[[k]]*log(mu_k(lst_t[[k]]))))
    
    sum_l_mk - sum(v_int_lambda) - sum(v_int_mu)
  }
  
  print(par)
  alpha = par[1:20]
  beta = par[21:40]
  gamma = par[41]
  
  ret = NULL
  for(k in 1:N) {
    ret[k] = log_lik_k(k)
  }
  -sum(ret)
}

set.seed(1)
mod_1_Rsolnp = solnp(pars = rep(1, 41), log_lik, LB = rep(0, 41), eqfun = function(par) par[1], eqB = 1)
save(mod_1_Rsolnp, file = "sol/mod_1_Rsolnp.RData")

