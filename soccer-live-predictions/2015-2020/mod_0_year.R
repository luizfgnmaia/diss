
library(CVXR)

load("2015-2020/data/input_year.RData")
load("2015-2020/data/input_mod_1_year.RData")

t0 = Sys.time()

alpha = Variable(n)
beta = Variable(n)
gamma = Variable(1)
tau = 0
omega = rep(0, 8)
theta = vstack(alpha, beta, gamma, tau, omega)

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

mod_0_year = list(alpha = as.vector(c(solution$getValue(alpha))),
                   beta = as.vector(solution$getValue(beta)),
                   gamma = as.vector(solution$getValue(gamma)),
                   loglik = solution$value,
                   duration = duration)
names(mod_0_year$alpha) = times$Time
names(mod_0_year$beta) = times$Time

save(mod_0_year, file = "2015-2020/data/mod_0_year.RData")

