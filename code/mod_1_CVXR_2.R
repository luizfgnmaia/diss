t0 = Sys.time()

library(CVXR)

load("serie_a_2019.RData")

alpha = Variable(20)
beta = Variable(20)
gamma = Variable(1)

lst_log_lik = list()
for(k in 1:N) { 
  lambda = gamma+alpha[i[k]]+beta[j[k]]
  mu = alpha[j[k]]+beta[i[k]]
  if(is.na(lst_J[[k]][1])) {
    sum_l_mk = 0
  } else {
    sum_l_mk = sum_entries((1-lst_J[[k]])*lambda+lst_J[[k]]*mu)
  }
  number_of_intervals = length(lst_int[[k]])-1
  lst_int_lambda = list()
  lst_int_mu = list()
  for(int in 1:number_of_intervals) {
    length_of_interval = lst_int[[k]][int+1]/90-lst_int[[k]][int]/90
    lst_int_lambda[[int]] = exp(lambda) * length_of_interval
    lst_int_mu[[int]] = exp(mu) * length_of_interval
  }
  int_lambda = sum_entries(do.call(vstack, lst_int_lambda))
  int_mu = sum_entries(do.call(vstack, lst_int_mu))
  lst_log_lik[[k]] = sum_l_mk - int_lambda - int_mu
} 

log_lik = sum_entries(do.call(vstack, lst_log_lik))
objective = Maximize(log_lik)
constraints = list(alpha[1] == 0)
problem = Problem(objective, constraints)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_1_CVXR = list(par = c(solution$getValue(alpha), solution$getValue(beta), solution$getValue(gamma)),
                  value = solution$value,
                  duration = duration)
save(mod_1_CVXR, file = "sol/mod_1_CVXR_2.RData")