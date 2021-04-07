
library(CVXR)
library(dplyr)
library(caret)

load("scrape/data/results.RData")

res = results %>%
  filter(Season == 2019) %>%
  arrange(Date)
dates = unique(res$Date)

mod_0_dates = list()

for(k in 2:length(dates)) {
  
  load("2019/data/input.RData")
  load("2019/data/input_mod_0.RData")
  
  lines = match_dates %>%
    filter(Date < dates[k])
  
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
  
  t0 = Sys.time()
  
  alpha = Variable(20)
  beta = Variable(20)
  gamma = Variable(1)
  theta = vstack(alpha, beta, gamma)
  
  log_lik = sum_entries(goals * M %*% theta - exp(M %*% theta))
  objective = Maximize(log_lik)
  constraints = list(sum(alpha) - sum(beta) == 0)
  problem = Problem(objective, constraints)
  solution = solve(problem, solver = "MOSEK")
  
  duration = Sys.time() - t0
  
  mod_0 = list(alpha = as.vector(c(solution$getValue(alpha))),
               beta = as.vector(solution$getValue(beta)),
               gamma = as.vector(solution$getValue(gamma)),
               value = solution$value,
               duration = duration)
  names(mod_0$alpha) = times$Time
  names(mod_0$beta) = times$Time
  
  mod_0_dates[[k]] = mod_0
  print(paste0(round(100*k/length(dates), 2), "%"))
}
names(mod_0_dates) = dates

save(mod_0_dates, file = "2019/data/mod_0_dates.RData")