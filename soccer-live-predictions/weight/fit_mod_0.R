
fit_mod_0 <- function(date, csi) {
  
  require(CVXR)
  
  load("2015-2020/data/input.RData")
  load("2015-2020/data/input_mod_0.RData")
  
  lines = match_dates %>%
    filter(Date < date)
  
  lines1 = lines$Lines1 %>%
    unlist()
  
  lines2 = lines$Lines2 %>%
    unlist()
  
  ind = lines$Ind
  
  M1 = M1[ind,]
  M2 = M2[ind,]
  M = rbind(M1, M2)
  x = x[ind]
  y = y[ind]
  goals = c(x, y)
  
  dif = as.integer(difftime(as.Date(date, "%Y-%m-%d"), as.Date(lines$Date, "%Y-%m-%d"), units = "days"))
  
  w = rep(exp(- csi * dif), 2)
  
  alpha = Variable(n)
  beta = Variable(n)
  gamma = Variable(1)
  theta = vstack(alpha, beta, gamma)
  
  log_lik = sum_entries((goals * M %*% theta - exp(M %*% theta)) * w)
  
  objective = Maximize(log_lik)
  constraints = list(sum(alpha) - sum(beta) == 0)
  problem = Problem(objective, constraints)
  solution = solve(problem, solver = "MOSEK")
  # if(solution$status == "solver_error") {
  #   solution = solve(problem, solver = "ECOS")
  #   message(paste0("Erro no MOSEK na data: ", date, ". ECOS teve solution$status: ", solution$status, "."))
  # }
  
  mod_0 = list(alpha = as.vector(c(solution$getValue(alpha))),
               beta = as.vector(solution$getValue(beta)),
               gamma = as.vector(solution$getValue(gamma)),
               value = solution$value)
  names(mod_0$alpha) = times$Time
  names(mod_0$beta) = times$Time
  
  mod_0
}



fit_mod_0_dates <- function(csi, verbose = FALSE) {
  require(dplyr)
  load("2015-2020/data/input.RData")
  
  dates = match_dates %>%
    filter(Season > 2015) %>%
    .$Date %>%
    unique()
  
  # lapply(dates, function(x) fit_mod_0(x, csi))
  ret = list()
  for(i in 1:length(dates)) { 
    ret[[i]] = fit_mod_0(dates[i], csi)
    if(verbose == TRUE) {
      print(paste0(round(100*i/length(dates), 2), "%"))
    }
  }
  names(ret) = dates
  ret
}
