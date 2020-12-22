
library(Rsolnp)

load("serie_a_2019.RData")

log_lik <- function(par) {
  
  log_lik_k <- function(k) { 
    
    lambda_xy <- function(t) {
      x = lst_x[[k]][round(90*t)+1] 
      y = lst_y[[k]][round(90*t)+1]  
      ifelse(x==1 & y==0, lambda_10,
             ifelse(x==0 & y==1, lambda_01,
                    ifelse(x+y>1 & x-y>=1, lambda_21,
                           ifelse(x+y>1 & x-y<=-1, lambda_12, 1))))
    }
    
    mu_xy <- function(t) {
      x = lst_x[[k]][round(90*t)+1] 
      y = lst_y[[k]][round(90*t)+1] 
      ifelse(x==1 & y==0, mu_10,
             ifelse(x==0 & y==1, mu_01,
                    ifelse(x+y>1 & x-y>=1, mu_21,
                           ifelse(x+y>1 & x-y<=-1, mu_12, 1))))
    }
    
    lambda_k <- function(t) {             
      ro_1^(t==0.5)*ro_2^(t==1)*lambda_xy(t-1/90)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*t
    }
    
    mu_k <- function(t) {             
      ro_1^(t==0.5)*ro_2^(t==1)*mu_xy(t-1/90)*alpha[j[k]]*beta[i[k]]+csi_2*t
    }
    
    int_lambda <- function(t1, t2) {        
      ro_1^(t2==0.5)*ro_2^(t2==1)*lambda_xy(t1)*gamma*alpha[i[k]]*beta[j[k]]*(t2-t1) + csi_1*(t2^2-t1^2)*0.5
    }
    
    int_mu <- function(t1, t2) {        
      ro_1^(t2==0.5)*ro_2^(t2==1)*mu_xy(t1)*alpha[j[k]]*beta[i[k]]*(t2-t1) + csi_2*(t2^2-t1^2)*0.5
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
  csi_1 = par[42]
  csi_2 = par[43]
  ro_1 = par[44]
  ro_2 = par[45]
  lambda_10 = par[46]
  lambda_01 = par[47]
  lambda_21 = par[48]
  lambda_12 = par[49]
  mu_10 = par[50]
  mu_01 = par[51]
  mu_21 = par[52]
  mu_12 = par[53]
  
  ret = NULL
  for(k in 1:N) {
    ret[k] = log_lik_k(k)
  }
  -sum(ret)
}

set.seed(1)
mod_5_Rsolnp = solnp(pars = rep(1, 53), log_lik, LB = rep(0, 53), eqfun = function(par) par[1], eqB = 1)
save(mod_5_Rsolnp, file = "sol/mod_5_Rsolnp.RData")


