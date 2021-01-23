
# Mudar a escala dos deltas tem bastante impacto nos betas

library(dplyr)
library(CVXR)

load("dados_serie_a_2019.RData")
load("dados_mod_1.RData")

# delta1 = delta1/90
# delta2 = delta2/90

t0 = Sys.time()

alpha = Variable(19)
beta = Variable(20)
gamma = Variable(1)
theta = vstack(alpha, beta, gamma)

tau = Variable(2)
phi = Variable(2)
omega = Variable(2)
pi1 = tau[1] + phi[1] * r1 + omega[1] * s1
pi2 = tau[2] + phi[2] * r2 + omega[2] * s2

log_lik = -t(delta1) %*% exp(M1_lambda %*% theta) - t(delta1) %*% exp(M1_mu %*% theta) - t(delta2) %*% exp(M2_lambda %*% theta) - t(delta2) %*% exp(M2_mu %*% theta) +
  t(1-J1) %*% G1_lambda %*% theta + t(J1) %*% G1_mu %*% theta + t(1-J2) %*% G2_lambda %*% theta + t(J2) %*% G2_mu %*% theta +
  t(T1) %*% log(pi1) + t(T2) %*% log(pi2) - sum_entries(pi1) - sum_entries(pi2)

objective = Maximize(log_lik)
problem = Problem(objective)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_1 = list(par = c(0, solution$getValue(alpha), solution$getValue(beta), solution$getValue(gamma),
                     solution$getValue(tau), solution$getValue(phi), solution$getValue(omega)),
                  value = solution$value,
                  duration = duration)
save(mod_1, file = "mod_1.RData")


library(ggplot2)

load("dixon robinson/sol/mod_1_CVXR.RData")

tibble(new = mod_1$par[1:41], old = mod_1_CVXR$par, par = c(rep("alpha", 20), rep("beta", 20), "gamma")) %>%
  ggplot(aes(x = new, y = old, col = par)) +
  geom_point(size = 2) +
  theme_bw()


