
library(CVXR)

load("2015-2020/data/input_year.RData")
load("2015-2020/data/input_mod_12_year.RData")

M1_lambda = M1_lambda[,1:(2*n+2)] 
M2_lambda = M2_lambda[,1:(2*n+2)] 
M1_mu = M1_mu[,1:(2*n+2)] 
M2_mu = M2_mu[,1:(2*n+2)] 

t0 = Sys.time()

alpha = Variable(n)
beta = Variable(n)
gamma = Variable(1)
tau = Variable(1)
theta = vstack(alpha, beta, gamma, tau)

loglambda1 = log(delta1) + M1_lambda %*% theta
logmu1 = log(delta1) + M1_mu %*% theta
loglambda2 = log(delta2) + M2_lambda %*% theta
logmu2 = log(delta2) + M2_mu %*% theta

log_lik_goals = sum_entries(
  - exp(loglambda1) - exp(logmu1) +
    H1r*loglambda1 + A1r*logmu1) +
  sum_entries(
    - exp(loglambda2) - exp(logmu2) +
      + H2r*loglambda2 + A2r*logmu2)

objective = Maximize(log_lik_goals)
constraints = list(sum(alpha) - sum(beta) == 0)
problem = Problem(objective, constraints)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_B_year = list(alpha = as.vector(c(solution$getValue(alpha))),
                   beta = as.vector(solution$getValue(beta)),
                   gamma = as.vector(solution$getValue(gamma)),
                   tau = as.vector(solution$getValue(tau)),
                   loglik = solution$value,
                   duration = duration)
names(mod_B_year$alpha) = times$Time
names(mod_B_year$beta) = times$Time

save(mod_B_year, file = "2015-2020/data/mod_B_year.RData")

