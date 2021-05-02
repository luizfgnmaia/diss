
# date = "2019-05-01"
# csi = 0.0065/3.5
# fit_mod_3(date, csi)

fit_mod_3 <- function(date, csi) {
  
  require(CVXR)
  
  load("2015-2020/data/input.RData")
  load("2015-2020/data/input_mod_2.RData")
  
  lines = match_dates %>%
    filter(Date < date)
  
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
  
  dates_1 = dates_1[which(dates_1 < date)]
  dates_2 = dates_2[which(dates_2 < date)]
  dates_1s = dates_1s[which(dates_1s < date)]
  dates_2s = dates_2s[which(dates_2s < date)]
  
  dif_1 = as.integer(difftime(as.Date(date, "%Y-%m-%d"), as.Date(dates_1, "%Y-%m-%d"), units = "days"))
  dif_2 = as.integer(difftime(as.Date(date, "%Y-%m-%d"), as.Date(dates_2, "%Y-%m-%d"), units = "days"))
  dif_1s = as.integer(difftime(as.Date(date, "%Y-%m-%d"), as.Date(dates_1s, "%Y-%m-%d"), units = "days"))
  dif_2s = as.integer(difftime(as.Date(date, "%Y-%m-%d"), as.Date(dates_2s, "%Y-%m-%d"), units = "days"))
  dif_st = as.integer(difftime(as.Date(date, "%Y-%m-%d"), as.Date(lines$Date, "%Y-%m-%d"), units = "days"))
  
  w_1 = exp(- csi * dif_1)
  w_2 = exp(- csi * dif_2)
  w_1s = exp(- csi * dif_1s)
  w_2s = exp(- csi * dif_2s)
  w_st = exp(- csi * dif_st)
  
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
  
  log_lik_goals = sum_entries((
    - exp(loglambda1) - exp(logmu1) +
      H1r*loglambda1 + A1r*logmu1) * w_1) +
    sum_entries((
      - exp(loglambda2) - exp(logmu2) +
        + H2r*loglambda2 + A2r*logmu2) * w_2)
  
  log_lik_reds = sum_entries((
    - exp(loglambda1s) - exp(logmu1s) +
      H1s*loglambda1s + A1s*logmu1s) * w_1s) +
    sum_entries((
      - exp(loglambda2s) - exp(logmu2s) +
        + H2s*loglambda2s + A2s*logmu2s) * w_2s)
  
  log_lik_st = sum_entries((U1 * log(pi1) + U2 * log(pi2) - pi1 - pi2) * w_st)
  
  log_lik = log_lik_goals + log_lik_reds + log_lik_st
  
  objective = Maximize(log_lik)
  constraints = list(sum(alpha) - sum(beta) == 0)
  problem = Problem(objective, constraints)
  solution = solve(problem, solver = "MOSEK")
  if(solution$status == "solver_error") {
    solution = solve(problem, solver = "ECOS")
    message(paste0("Erro no MOSEK na data: ", date, ". ECOS teve solution$status: ", solution$status, "."))
  }
  
  mod_3 = list(alpha = as.vector(c(solution$getValue(alpha))),
               beta = as.vector(solution$getValue(beta)),
               gamma = as.vector(solution$getValue(gamma)),
               tau = as.vector(solution$getValue(tau)),
               omega = as.vector(solution$getValue(omega)),
               a = as.vector(solution$getValue(a)),
               eta = as.vector(solution$getValue(eta)),
               rho = as.vector(solution$getValue(rho)),
               kappa = as.vector(solution$getValue(kappa)),
               loglik = solution$value)
  names(mod_3$alpha) = times$Time
  names(mod_3$beta) = times$Time
  names(mod_3$omega) = c("goal", "red")
  names(mod_3$a) = c("lambda", "mu")
  
  mod_3
}



fit_mod_3_dates <- function(csi) {
  require(dplyr)
  load("2015-2020/data/input.RData")
  
  dates = match_dates %>%
    filter(Season > 2015) %>%
    .$Date %>%
    unique()
  
  # lapply(dates, function(x) fit_mod_3(x, csi))
  ret = list()
  for(i in 1:length(dates)) { 
    ret[[i]] = fit_mod_3(dates[i], csi)
    print(paste0(round(100*i/length(dates), 2), "%"))
  }
  names(ret) = dates
  ret
}
