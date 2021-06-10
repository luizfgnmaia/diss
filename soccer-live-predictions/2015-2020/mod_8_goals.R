
library(CVXR)

load("2015-2020/data/input.RData")
load("2015-2020/data/input_mod_7.RData")

t0 = Sys.time()

alpha = Variable(n)
beta = Variable(n)
gamma = Variable(1)
tau = Variable(1)
omega_xy = Variable(2)
omega = Variable(1)
theta = vstack(alpha, beta, gamma, tau, omega_xy[1], omega_xy[2], omega_xy[2], omega_xy[1], omega, omega)

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

mod_8_goals = list(alpha = as.vector(c(solution$getValue(alpha))),
                   beta = as.vector(solution$getValue(beta)),
                   gamma = as.vector(solution$getValue(gamma)),
                   tau = as.vector(solution$getValue(tau)),
                   omega_xy = as.vector(solution$getValue(omega_xy)),
                   omega = as.vector(solution$getValue(omega)),
                   loglik = solution$value,
                   duration = duration)
names(mod_8_goals$alpha) = times$Time
names(mod_8_goals$beta) = times$Time
names(mod_8_goals$omega_xy) = c("ahead", "behind")
names(mod_8_goals$omega) = "red"

save(mod_8_goals, file = "2015-2020/data/mod_8_goals.RData")

