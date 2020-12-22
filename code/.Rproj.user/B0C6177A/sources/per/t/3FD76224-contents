
library(CVXR)

load("serie_a_2019.RData")

t0 = Sys.time()

alpha = Variable(19)
beta = Variable(20)
gamma = Variable(1)
lambda_xy = Variable(2)
mu_xy = Variable(2)
theta = vstack(alpha, beta, gamma, lambda_xy, mu_xy)

log_lik = - t(delta) %*% exp(M1_mod2 %*% theta) - t(delta) %*% exp(M2_mod2 %*% theta) + t(H) %*% (M1_mod2 %*% theta) + t(A) %*% (M2_mod2 %*% theta)
objective = Maximize(log_lik)
problem = Problem(objective)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_2_CVXR = list(par = c(0, solution$getValue(alpha), solution$getValue(beta), solution$getValue(gamma),
                          solution$getValue(lambda_xy), solution$getValue(mu_xy)),
                  value = solution$value,
                  duration = duration)
save(mod_2_CVXR, file = "sol/mod_2_CVXR.RData")

load("sol/mod_2_Rsolnp.RData")
plot(exp(mod_2_CVXR$par), mod_2_Rsolnp$pars)
cor(exp(mod_2_CVXR$par), mod_2_Rsolnp$pars)
