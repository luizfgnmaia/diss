
library(CVXR)
library(dplyr)

load("scrape/data/results.RData")

res = results %>%
  filter(Season == 2020) %>%
  arrange(Date)
dates = unique(res$Date)

mod_3 = list()

for(k in 2:length(dates)) {
  
  load("2020/data/input.RData")
  load("2020/data/input_mod_2.RData")
  
  lines = match_dates %>%
    filter(Date < dates[k])
  
  lines1 = lines$Lines1 %>%
    unlist()
  
  lines2 = lines$Lines2 %>%
    unlist()
  
  lines1s = lines$Lines1s %>%
    unlist()
  
  lines2s = lines$Lines2s %>%
    unlist()
  
  M1_lambda = M1_lambda[lines1,]
  M1_mu = M1_mu[lines1,]
  M2_lambda = M2_lambda[lines2,]
  M2_mu = M2_mu[lines2,]
  delta1 = delta1[lines1]
  delta2 = delta2[lines2]
  H1r = H1r[lines1]
  H2r = H2r[lines2]
  A1r = A1r[lines1]
  A2r = A2r[lines2]
  
  delta1s = delta1s[lines1s]
  delta2s = delta2s[lines2s]
  int_reds_1 = int_reds_1[lines1s]
  int_reds_2 = int_reds_2[lines2s]
  H1s = H1s[lines1s]
  H2s = H2s[lines2s]
  A1s = A1s[lines1s]
  A2s = A2s[lines2s]
  
  ind = lines$Ind
  
  U1 = U1[ind]
  U2 = U2[ind]
  r1 = r1[ind]
  r2 = r2[ind]
  c = c[ind]
  
  t0 = Sys.time()
  
  alpha = Variable(n)
  beta = Variable(n)
  gamma = Variable(1)
  tau = Variable(1)
  omega = Variable(2)
  theta = vstack(alpha, beta, gamma, tau, omega[1], omega[1], omega[2], omega[2])
  
  eta = Variable(2)
  rho = Variable(2)
  kappa = Variable(1)
  pi1 = eta[1] + rho[1] * r1
  pi2 = eta[2] + rho[2] * r2 + c * kappa
  
  a = Variable(2)
  
  loglambda1 = log(delta1) + M1_lambda %*% theta
  logmu1 = log(delta1) + M1_mu %*% theta
  loglambda2 = log(delta2) + M2_lambda %*% theta
  logmu2 = log(delta2) + M2_mu %*% theta
  
  loglambda1s = log(int_reds_1) + a[1]
  logmu1s = log(int_reds_1) + a[2]
  loglambda2s = log(int_reds_2) + a[1]
  logmu2s = log(int_reds_2) + a[2]
  
  log_lik_goals = sum_entries(
    - exp(loglambda1) - exp(logmu1) +
      H1r*loglambda1 + A1r*logmu1) +
    sum_entries(
      - exp(loglambda2) - exp(logmu2) +
        + H2r*loglambda2 + A2r*logmu2)
  
  log_lik_reds = sum_entries(
    - exp(loglambda1s) - exp(logmu1s) +
      H1s*loglambda1s + A1s*logmu1s) +
    sum_entries(
      - exp(loglambda2s) - exp(logmu2s) +
        + H2s*loglambda2s + A2s*logmu2s)
  
  log_lik_st = t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum_entries(pi1) - sum_entries(pi2)   
  
  log_lik = log_lik_goals + log_lik_reds + log_lik_st
  
  objective = Maximize(log_lik)
  constraints = list(sum(alpha) - sum(beta) == 0)
  problem = Problem(objective, constraints)
  solution = solve(problem, solver = "MOSEK")
  
  duration = Sys.time() - t0
  
  m3 = list(alpha = as.vector(c(solution$getValue(alpha))),
            beta = as.vector(solution$getValue(beta)),
            gamma = as.vector(solution$getValue(gamma)),
            tau = as.vector(solution$getValue(tau)),
            omega = as.vector(solution$getValue(omega)),
            a = as.vector(solution$getValue(a)),
            eta = as.vector(solution$getValue(eta)),
            rho = as.vector(solution$getValue(rho)),
            kappa = as.vector(solution$getValue(kappa)),
            loglik = solution$value,
            duration = duration)
  names(m3$alpha) = times$Time
  names(m3$beta) = times$Time
  names(m3$omega) = c("goal", "red")
  names(m3$a) = c("lambda", "mu")
  
  mod_3[[k]] = m3
  print(paste0(round(100*k/length(dates), 2), "%"))
}
names(mod_3) = dates

save(mod_3, file = "pred 2020/data/mod_3.RData")