
library(CVXR)

load("serie_a_2019.RData")

log_lik <- function(alpha, beta, gamma) {
  lambda = alpha[i] + beta[j] + gamma
  mu = alpha[j] + beta[i]
  sum(-exp(lambda) + x*lambda - exp(mu) + y*mu)
}

alpha = Variable(20)
beta = Variable(20)
gamma = Variable(1)

objective = Maximize(log_lik(alpha, beta, gamma))
constraints = list(alpha[1] == 0)
problem = Problem(objective, constraints)
set.seed(1)
mod_0_CVXR = solve(problem)

save(mod_0_CVXR, file = "sol/mod_0_CVXR.RData")

