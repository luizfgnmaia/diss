
library(CVXR)

load("2020/data/input.RData")
load("2020/data/input_mod_r1.RData")

t0 = Sys.time()

alpha = Variable(n)
beta = Variable(n)
gamma = Variable(1)
tau = Variable(1)
omega = Variable(8)
theta = vstack(alpha, beta, gamma, tau, omega)

eta = Variable(2)
phi = Variable(2)
rho = Variable(2)
kappa = Variable(1)
pi1 = eta[1] + phi[1] * g1 + rho[1] * r1
pi2 = eta[2] + phi[2] * g2 + rho[2] * r2 + c * kappa

a = Variable(2)

loglambda1 = log(delta1) + M1_lambda %*% theta
logmu1 = log(delta1) + M1_mu %*% theta
loglambda2 = log(delta2) + M2_lambda %*% theta
logmu2 = log(delta2) + M2_mu %*% theta

loglambda1s = log(int_reds_1) + a[1]
logmu1s = log(int_reds_1) + a[1]
loglambda2s = log(int_reds_2) + a[2]
logmu2s = log(int_reds_2) + a[2]

log_lik_goals = - sum_entries(exp(loglambda1)) - sum_entries(exp(logmu1)) - sum_entries(exp(loglambda2)) - sum_entries(exp(logmu2)) +
  sum_entries(H1r*loglambda1) + sum_entries(A1r*logmu1) + sum_entries(H2r*loglambda2) + sum_entries(A2r*logmu2)

log_lik_reds = - sum_entries(exp(loglambda1s)) - sum_entries(exp(logmu1s)) - sum_entries(exp(loglambda2s)) - sum_entries(exp(logmu2s)) +
  sum_entries(H1s*loglambda1s) + sum_entries(A1s*logmu1s) + sum_entries(H2s*loglambda2s) + sum_entries(A2s*logmu2s)
  
log_lik_st = t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum_entries(pi1) - sum_entries(pi2)   
  
log_lik = log_lik_goals + log_lik_reds + log_lik_st

objective = Maximize(log_lik)
constraints = list(sum(alpha) - sum(beta) == 0)
problem = Problem(objective, constraints)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_r1 = list(alpha = as.vector(c(solution$getValue(alpha))),
             beta = as.vector(solution$getValue(beta)),
             gamma = as.vector(solution$getValue(gamma)),
             tau = as.vector(solution$getValue(tau)),
             omega = as.vector(solution$getValue(omega)),
             a = as.vector(solution$getValue(a)),
             eta = as.vector(solution$getValue(eta)),
             phi = as.vector(solution$getValue(phi)),
             rho = as.vector(solution$getValue(rho)),
             kappa = as.vector(solution$getValue(kappa)),
             loglik = solution$value + sum(log(factorial(U1))) + sum(log(factorial(U2))),
             duration = duration)
names(mod_r1$alpha) = times$Time
names(mod_r1$beta) = times$Time
names(mod_r1$a) = c("a_lambda", "a_mu")
names(mod_r1$omega) = c("lambda_x", "lambda_y", "mu_x", "mu_y",
                       "lambda_x^s", "lambda_y^s", "mu_x^s", "mu_y^s")

save(mod_r1, file = "2020/data/mod_r1.RData")

