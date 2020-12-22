library(Rsolnp)

load("serie_a_2019.RData")

log_lik <- function(par) {
  
  log_lik_k <- function(k) { 
    
    lambda_xy <- function(t) {
      x = lst_x[[k]][round(90*t)+1] 
      y = lst_y[[k]][round(90*t)+1]  
      ifelse(x==y, 1,
             ifelse(x-y>=1, lambda_10, lambda_01))
    }
    
    mu_xy <- function(t) {
      x = lst_x[[k]][round(90*t)+1] 
      y = lst_y[[k]][round(90*t)+1] 
      ifelse(x==y, 1,
             ifelse(x-y>=1, mu_10, mu_01))
    }
    
    lambda_k <- function(t) {             
      ro_1^(t==0.5)*ro_2^(t==1)*lambda_xy(t-1/90)*gamma*alpha[i[k]]*beta[j[k]]
    }
    
    mu_k <- function(t) {             
      ro_1^(t==0.5)*ro_2^(t==1)*mu_xy(t-1/90)*alpha[j[k]]*beta[i[k]]
    }
    
    int_lambda <- function(t1, t2) {        
      ro_1^(t2==0.5)*ro_2^(t2==1)*lambda_xy(t1)*gamma*alpha[i[k]]*beta[j[k]]*(t2-t1)
    }
    
    int_mu <- function(t1, t2) {        
      ro_1^(t2==0.5)*ro_2^(t2==1)*mu_xy(t1)*alpha[j[k]]*beta[i[k]]*(t2-t1)
    }
    
    v_int_lambda = NULL 
    v_int_mu = NULL 
    for(int in 1:(length(lst_int_st[[k]])-1)) {
      v_int_lambda[int] = int_lambda(t1 = lst_int_st[[k]][int]/90, t2 = lst_int_st[[k]][int+1]/90)
      v_int_mu[int] = int_mu(t1 = lst_int_st[[k]][int]/90, t2 = lst_int_st[[k]][int+1]/90)
    }
    
    sum_l_mk = ifelse(is.na(lst_J[[k]][1]), 0, 
                      sum((1-lst_J[[k]])*log(lambda_k(lst_t[[k]]))+lst_J[[k]]*log(mu_k(lst_t[[k]]))))
    
    sum_l_mk - sum(v_int_lambda) - sum(v_int_mu)
  }
  
  print(par)
  alpha = par[1:20]
  beta = par[21:40]
  gamma = par[41]
  lambda_10 = par[42]
  lambda_01 = par[43]
  mu_10 = par[44]
  mu_01 = par[45]
  ro_1 = par[46]
  ro_2 = par[47]
  
  ret = NULL
  for(k in 1:N) {
    ret[k] = log_lik_k(k)
  }
  -sum(ret)
}

set.seed(1)
mod_3_Rsolnp = solnp(pars = rep(1, 47), log_lik, LB = rep(0, 47), eqfun = function(par) par[1], eqB = 1)
save(mod_3_Rsolnp, file = "sol/mod_3_Rsolnp.RData")