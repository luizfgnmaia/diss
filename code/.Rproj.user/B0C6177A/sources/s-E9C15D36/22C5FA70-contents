
library(dplyr)
library(CVXR)

load("dados_serie_a_2019.RData")
load("dados_mod_2.RData")

# delta1 = delta1/100
# delta2 = delta2/100

t0 = Sys.time()

alpha = Variable(19)
beta = Variable(20)
gamma = Variable(1)
lambda_xy = Variable(2)
mu_xy = Variable(2)
theta = vstack(alpha, beta, gamma, lambda_xy, mu_xy)

tau = Variable(2)
phi = Variable(2)
kappa = Variable(1)
pi1 = tau[1] + phi[1] * r1
pi2 = tau[2] + phi[2] * r2 + c * kappa

lambda1 = delta1 * exp(M1_lambda %*% theta)
mu1 = delta1 * exp(M1_mu %*% theta) 
lambda2 = delta2 * exp(M2_lambda %*% theta) 
mu2 = delta2 * exp(M2_mu %*% theta)

loglambda1 = log(delta1) + M1_lambda %*% theta
logmu1 = log(delta1) + M1_mu %*% theta
loglambda2 = log(delta2) + M2_lambda %*% theta
logmu2 = log(delta2) + M2_mu %*% theta

log_lik = - sum_entries(lambda1) - sum_entries(mu1) - sum_entries(lambda2) - sum_entries(mu2) +
  sum_entries(H1*loglambda1) + sum_entries(A1*logmu1) + sum_entries(H2*loglambda2) + sum_entries(A2*logmu2) +
  t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum_entries(pi1) - sum_entries(pi2)

objective = Maximize(log_lik)
problem = Problem(objective)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_2 = list(alpha = as.vector(c(0, solution$getValue(alpha))),
             beta = as.vector(solution$getValue(beta)),
             gamma = as.vector(solution$getValue(gamma)),
             lambda_xy = as.vector(solution$getValue(lambda_xy)),
             mu_xy = as.vector(solution$getValue(mu_xy)),
             tau = as.vector(solution$getValue(tau)),
             phi = as.vector(solution$getValue(phi)),
             kappa = as.vector(solution$getValue(kappa)),
             value = solution$value,
             duration = duration)
names(mod_2$alpha) = times$Time
names(mod_2$beta) = times$Time
names(mod_2$lambda_xy) = c("10", "01")
names(mod_2$mu_xy) = c("10", "01")

# save(mod_2, file = "mod_2.RData")

# library(ggplot2)
# 
# load("dixon robinson/sol/mod_2_CVXR.RData")
# 
# tibble(new = mod_2$par$val[1:41], old = mod_2_CVXR$par[1:41], 
#        par = c(rep("alpha", 20), rep("beta", 20), "gamma")) %>%
#   ggplot(aes(x = new, y = old, col = par)) +
#   geom_point(size = 2) +
#   theme_bw()
# 
# mod_2$par$val[42:45]
# mod_2_CVXR$par[42:45]
# 
# load("mod_1.RData")
# mod_2$par$val[46:51]
# mod_1$par$val[42:47]
# 
# rank(mod_2$par$val[1:20])
# rank(mod_2_CVXR$par[1:20])
# 
# rank(mod_2$par$val[21:40])
# rank(mod_2_CVXR$par[21:40])
