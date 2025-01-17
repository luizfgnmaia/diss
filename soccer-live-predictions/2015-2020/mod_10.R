
library(CVXR)

load("2015-2020/data/input.RData")
load("2015-2020/data/input_mod_10.RData")

t0 = Sys.time()

alpha = Variable(n)
beta = Variable(n)
gamma = Variable(1)
tau = Variable(1)
omega_xy = Variable(2)
omega = Variable(2)
theta = vstack(alpha, beta, gamma, tau, omega_xy[1], omega_xy[2], omega_xy[2], omega_xy[1], omega[1], omega[1], omega[2])

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

mod_10 = list(alpha = as.vector(c(solution$getValue(alpha))),
             beta = as.vector(solution$getValue(beta)),
             gamma = as.vector(solution$getValue(gamma)),
             tau = as.vector(solution$getValue(tau)),
             omega_xy = as.vector(solution$getValue(omega_xy)),
             omega = as.vector(solution$getValue(omega)),
             a = as.vector(solution$getValue(a)),
             eta = as.vector(solution$getValue(eta)),
             rho = as.vector(solution$getValue(rho)),
             kappa = as.vector(solution$getValue(kappa)),
             loglik = solution$value - sum(log(factorial(U1))) - sum(log(factorial(U2))),
             duration = duration)
names(mod_10$alpha) = times$Time
names(mod_10$beta) = times$Time
names(mod_10$omega_xy) = c("ahead", "behind")
names(mod_10$omega) = c("red", "value")
names(mod_10$a) = c("lambda", "mu")

save(mod_10, file = "2015-2020/data/mod_10.RData")

