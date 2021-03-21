
library(CVXR)

load("2020/data/input.RData")
load("2020/data/input_mod_2.RData")

t0 = Sys.time()

alpha = Variable(n)
beta = Variable(n)
gamma = Variable(1)
tau = Variable(1)
theta = vstack(alpha, beta, gamma, tau)

eta = Variable(2)
phi = Variable(2)
kappa = Variable(1)
pi1 = eta[1] + phi[1] * g1
pi2 = eta[2] + phi[2] * g2 + c * kappa

loglambda1 = log(delta1) + M1_lambda %*% theta
logmu1 = log(delta1) + M1_mu %*% theta
loglambda2 = log(delta2) + M2_lambda %*% theta
logmu2 = log(delta2) + M2_mu %*% theta

log_lik = - sum_entries(exp(loglambda1)) - sum_entries(exp(logmu1)) - sum_entries(exp(loglambda2)) - sum_entries(exp(logmu2)) +
  sum_entries(H1*loglambda1) + sum_entries(A1*logmu1) + sum_entries(H2*loglambda2) + sum_entries(A2*logmu2) +
  t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum_entries(pi1) - sum_entries(pi2)

objective = Maximize(log_lik)
constraints = list(sum(alpha) - sum(beta) == 0)
problem = Problem(objective, constraints)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_2 = list(alpha = as.vector(c(solution$getValue(alpha))),
             beta = as.vector(solution$getValue(beta)),
             gamma = as.vector(solution$getValue(gamma)),
             tau = as.vector(solution$getValue(tau)),
             eta = as.vector(solution$getValue(eta)),
             phi = as.vector(solution$getValue(phi)),
             kappa = as.vector(solution$getValue(kappa)),
             loglik = solution$value + sum(log(factorial(U1))) + sum(log(factorial(U2))),
             duration = duration)
names(mod_2$alpha) = times$Time
names(mod_2$beta) = times$Time

save(mod_2, file = "2020/data/mod_2.RData")
