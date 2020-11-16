
library(CVXR)

load("serie_a_2019.RData")

log_lik <- function(alpha, beta, gamma, csi, ro) {
  
  log_lik_k <- function(k) { 
    
    lambda_xy <- function(t) {
      1
    }
    
    mu_xy <- function(t) {
      1
    }
    
    lambda_k <- function(t) {
      log(lambda_xy(t))+ro[1]*(t==0.5)+ro[2]*(t==1)+gamma+alpha[i[k]]+beta[j[k]]+csi[1]*t
    }
    
    mu_k <- function(t) {
      log(mu_xy(t))+ro[1]*(t==0.5)+ro[2]*(t==1)+alpha[j[k]]+beta[i[k]]+csi[2]*t
    }
    
    int_lambda <- function(t1, t2) { 
      exp(log(lambda_xy(t1))+ro[1]*(t==0.5)+ro[2]*(t==1)+gamma+alpha[i[k]]+beta[j[k]])*(t2-t1) + csi[1]*(t2^2-t1^2)*0.5
    }
    
    int_mu <- function(t1, t2) { 
      exp(log(mu_xy(t1))+ro[1]*(t==0.5)+ro[2]*(t==1)+alpha[j[k]]+beta[i[k]])*(t2-t1) + csi[2]*(t2^2-t1^2)*0.5
    }
    
    int_lambda_01 = 0 
    int_mu_01 = 0 
    for(int in 1:(length(lst_int[[k]])-1)) {
      int_lambda_01 = int_lambda_01 + int_lambda(t1 = lst_int[[k]][int]/90, t2 = lst_int[[k]][int+1]/90)
      int_mu_01 = int_mu_01 + int_mu(t1 = lst_int[[k]][int]/90, t2 = lst_int[[k]][int+1]/90)
    }
    
    if(is.na(lst_J[[k]][1])) {  
      sum_l_mk = 0
    } else {
      sum_l_mk = sum((1-lst_J[[k]])*lambda_k(lst_t[[k]])+lst_J[[k]]*mu_k(lst_t[[k]]))
    }
    sum_l_mk - int_lambda_01 - int_mu_01
  }
  
  ret = 0
  for(k in 1:N) {
    ret = ret + log_lik_k(k)
  }
  ret
}

alpha = Variable(20)
beta = Variable(20)
gamma = Variable(1)
csi = Variable(2)
ro = Variable(2)

objective = Maximize(log_lik(alpha, beta, gamma, csi, ro))
problem = Problem(objective)
set.seed(1)
solution = solve(problem)

mod_3_CVXR = list(par = c(solution$getValue(alpha), solution$getValue(beta), solution$getValue(gamma),
                          solution$getValue(csi), solution$getValue(ro)),
                  value = solution$value)
save(mod_2_CVXR, file = "sol/mod_3_CVXR.RData")


