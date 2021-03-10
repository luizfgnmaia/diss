
library(CVXR)

load("2019/data/input.RData")
load("2019/data/input_mod_3.RData")

t0 = Sys.time()

alpha = Variable(n)
beta = Variable(n)
gamma = Variable(1)
tau = Variable(1)
lambda_xy = Variable(2)
mu_xy = Variable(2)
theta = vstack(alpha, beta, gamma, tau, lambda_xy, mu_xy)

eta = Variable(2)
phi = Variable(2)
kappa = Variable(1)
pi1 = eta[1] + phi[1] * g1
pi2 = eta[2] + phi[2] * g2 + c * kappa

log_lik = -t(delta1)%*%exp(M1_lambda%*%theta) -t(delta1)%*%exp(M1_mu%*%theta) -t(delta2)%*%exp(M2_lambda%*%theta) -t(delta2)%*%exp(M2_mu%*%theta) +
  t(H1)%*%M1_lambda%*%theta + t(A1)%*%M1_mu%*%theta + t(H2)%*%M2_lambda%*%theta + t(A2)%*%M2_mu%*%theta +
  t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum_entries(pi1) - sum_entries(pi2)

objective = Maximize(log_lik)
constraints = list(sum(alpha) - sum(beta) == 0)
problem = Problem(objective, constraints)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_3 = list(alpha = as.vector(c(solution$getValue(alpha))),
             beta = as.vector(solution$getValue(beta)),
             gamma = as.vector(solution$getValue(gamma)),
             tau = as.vector(solution$getValue(tau)),
             lambda_xy = as.vector(solution$getValue(lambda_xy)),
             mu_xy = as.vector(solution$getValue(mu_xy)),
             eta = as.vector(solution$getValue(eta)),
             phi = as.vector(solution$getValue(phi)),
             kappa = as.vector(solution$getValue(kappa)),
             value = solution$value,
             duration = duration)
names(mod_3$alpha) = times$Time
names(mod_3$beta) = times$Time
names(mod_3$lambda_xy) = c("10", "01")
names(mod_3$mu_xy) = c("10", "01")

save(mod_3, file = "2019/data/mod_3.RData")

