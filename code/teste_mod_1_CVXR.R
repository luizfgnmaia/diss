t0 = Sys.time()

library(CVXR)

load("serie_a_2019.RData")

alpha = Variable(20)
beta = Variable(20)
gamma = Variable(1)

log_lik_k <- function(k) { 
  
  int_lambda_01 = 0 
  int_mu_01 = 0 
  for(int in 1:(length(lst_int[[k]])-1)) {
    int_lambda_01 = int_lambda_01 + exp(gamma+alpha[i[k]]+beta[j[k]]) * (lst_int[[k]][int+1]/90-lst_int[[k]][int]/90)
    int_mu_01 = int_mu_01 + exp(alpha[j[k]]+beta[i[k]]) * (lst_int[[k]][int+1]/90-lst_int[[k]][int]/90)
  }
  
  if(is.na(lst_J[[k]][1])) {  
    sum_l_mk = 0
  } else {
    sum_l_mk = sum((1-lst_J[[k]])*(gamma+alpha[i[k]]+beta[j[k]])+lst_J[[k]]*(alpha[j[k]]+beta[i[k]]))
  }
  sum_l_mk - int_lambda_01 - int_mu_01
}

lst_log_lik = lapply(1:N, log_lik_k)
expr = paste0("obj = ", paste(paste0("lst_log_lik[[", 1:N, "]]") , collapse = " + "))
eval(parse(text = expr))

objective = Maximize(obj)
constraints = list(alpha[1] == 0)
problem = Problem(objective, constraints)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

teste_mod_1_CVXR = list(par = c(solution$getValue(alpha), solution$getValue(beta), solution$getValue(gamma)),
                  value = solution$value,
                  duration = duration)
save(teste_mod_1_CVXR, file = "sol/teste_mod_1_CVXR.RData")