
library(dplyr)
library(CVXR)

load("dados_serie_a_2019.RData")
load("dados_mod_3.RData")

# delta1 = delta1/100
# delta2 = delta2/100

t0 = Sys.time()

alpha = Variable(19)
beta = Variable(20)
gamma = Variable(1)
lambda_xy = Variable(2)
mu_xy = Variable(2)
theta = vstack(alpha, beta, gamma, lambda_xy, mu_xy)

csi = Variable(2)

tau = Variable(2)
phi = Variable(2)
kappa = Variable(1)
pi1 = tau[1] + phi[1] * r1
pi2 = tau[2] + phi[2] * r2 + c * kappa

lambda1 = delta1 * exp(M1_lambda %*% theta) + exp(csi[1]) * delta_quad1/2
mu1 = delta1 * exp(M1_mu %*% theta) + exp(csi[2]) * delta_quad1/2
lambda2 = delta2 * exp(M2_lambda %*% theta) + exp(csi[1]) * delta_quad2/2
mu2 = delta2 * exp(M2_mu %*% theta) + exp(csi[2]) * delta_quad2/2


# Não é exatamente log_sum_exp que quero porque tem os intervalos das integrais!!!!!

# loglambda1 = log_sum_exp(hstack(exp(M1_lambda%*%theta), exp(csi[1])*delta_quad1/2), 1)
# logmu1 = log_sum_exp(hstack(delta1*exp(M1_mu%*%theta), exp(csi[2])*delta_quad1/2), 1)
# loglambda2 = log_sum_exp(hstack(delta2*exp(M2_lambda%*%theta), exp(csi[1])*delta_quad2/2), 1)
# logmu2 = log_sum_exp(hstack(delta2*exp(M2_mu%*%theta), exp(csi[2])*delta_quad2/2), 1)

# log_lik = - sum_entries(lambda1) - sum_entries(mu1) - sum_entries(lambda2) - sum_entries(mu2) +
#   sum_entries(H1*loglambda1) + sum_entries(A1*logmu1) + sum_entries(H2*loglambda2) + sum_entries(A2*logmu2) +
#   t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum_entries(pi1) - sum_entries(pi2)





# https://ajfriendcvxpy.readthedocs.io/en/latest/tutorial/dcp/

expr1 = -sum_entries(lambda1) -sum_entries(mu1) -sum_entries(lambda2) -sum_entries(mu2)
expr2 = sum_entries(H1*loglambda1) + sum_entries(A1*logmu1) + sum_entries(H2*loglambda2) + sum_entries(A2*logmu2)

is_dcp(expr1) # TRUE
is_dcp(expr2) # TRUE
is_dcp(expr1 + expr2) # FALSE
is_dcp(sum_entries(vstack(expr1, expr2))) # FALSE
is_dcp(sum(vstack(expr1, expr2))) # FALSE

curvature(expr1) # "CONCAVE"
curvature(expr2) # "CONVEX"
curvature(expr1 + expr2) # "UNKNOWN"







objective = Maximize(log_lik)
problem = Problem(objective)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

# mod_3 = list(alpha = as.vector(c(0, solution$getValue(alpha))),
#              beta = as.vector(solution$getValue(beta)),
#              gamma = as.vector(solution$getValue(gamma)),
#              lambda_xy = as.vector(solution$getValue(lambda_xy)),
#              mu_xy = as.vector(solution$getValue(mu_xy)),
#              csi = as.vector(solution$getValue(csi)),
#              tau = as.vector(solution$getValue(tau)),
#              phi = as.vector(solution$getValue(phi)),
#              kappa = as.vector(solution$getValue(kappa)),
#              value = solution$value,
#              duration = duration)
# names(mod_3$alpha) = times$Time
# names(mod_3$beta) = times$Time
# names(mod_3$lambda_xy) = c("10", "01")
# names(mod_3$mu_xy) = c("10", "01")

# save(mod_3, file = "mod_3.RData")

solution
