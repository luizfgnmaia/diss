
library(dplyr)
library(CVXR)

load("dados_serie_a_2019.RData")
load("dados_mod_4.RData")

t0 = Sys.time()

alpha = Variable(n-1)
beta = Variable(n)
gamma = Variable(1)
tau = Variable(1)
lambda_xy = Variable(2)
mu_xy = Variable(2)
theta = vstack(alpha, beta, gamma, tau, lambda_xy, mu_xy)
csi = Variable(2)

eta = Variable(2)
phi = Variable(2)
kappa = Variable(1)
pi1 = eta[1] + phi[1] * g1
pi2 = eta[2] + phi[2] * g2 + c * kappa

loglambda1 = M1_lambda %*% theta + log_sum_exp(hstack(t2_1*csi[1], -t1_1*csi[1]), axis = 1) - log(csi[1])
logmu1 = M1_mu %*% theta + log_sum_exp(hstack(t2_1*csi[1], -t1_1*csi[2]), axis = 1) - log(csi[2])
loglambda2 = M2_lambda %*% theta + log_sum_exp(hstack(t2_2*csi[1], -t1_2*csi[1]), axis = 1) - log(csi[1])
logmu2 = M2_mu %*% theta + log_sum_exp(hstack(t2_2*csi[2], -t1_2*csi[2]), axis = 1) - log(csi[2])

lambda1 = exp(loglambda1)
mu1 = exp(logmu1)
lambda2 = exp(loglambda2)
mu2 = exp(logmu2)

log_lik = - sum_entries(lambda1) - sum_entries(mu1) - sum_entries(lambda2) - sum_entries(mu2) +
  sum_entries(H1*loglambda1) + sum_entries(A1*logmu1) + sum_entries(H2*loglambda2) + sum_entries(A2*logmu2) +
  t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum_entries(pi1) - sum_entries(pi2)

objective = Maximize(log_lik)
problem = Problem(objective)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_4 = list(alpha = as.vector(c(0, solution$getValue(alpha))),
             beta = as.vector(solution$getValue(beta)),
             gamma = as.vector(solution$getValue(gamma)),
             tau = as.vector(solution$getValue(tau)),
             lambda_xy = as.vector(solution$getValue(lambda_xy)),
             mu_xy = as.vector(solution$getValue(mu_xy)),
             csi = as.vector(solution$getValue(csi)),
             eta = as.vector(solution$getValue(eta)),
             phi = as.vector(solution$getValue(phi)),
             kappa = as.vector(solution$getValue(kappa)),
             value = solution$value,
             duration = duration)
names(mod_4$alpha) = times$Time
names(mod_4$beta) = times$Time
names(mod_4$lambda_xy) = c("10", "01")
names(mod_4$mu_xy) = c("10", "01")

# save(mod_4, file = "mod_4.RData")
