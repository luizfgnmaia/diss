
library(CVXR)

load("serie_a_2019.RData")

t0 = Sys.time()

alpha = Variable(19)
beta = Variable(20)
gamma = Variable(1)
theta = vstack(alpha, beta, gamma)

log_lik = - t(delta) %*% exp(M1_mod1 %*% theta) - t(delta) %*% exp(M2_mod1 %*% theta) + t(H) %*% (M1_mod1 %*% theta) + t(A) %*% (M2_mod1 %*% theta)
objective = Maximize(log_lik)
problem = Problem(objective)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_1_CVXR = list(par = c(0, solution$getValue(alpha), solution$getValue(beta), solution$getValue(gamma)),
                  value = solution$value,
                  duration = duration)
save(mod_1_CVXR, file = "sol/mod_1_CVXR.RData")

load("sol/mod_1_Rsolnp.RData")
plot(exp(mod_1_CVXR$par), mod_1_Rsolnp$pars)
cor(exp(mod_1_CVXR$par), mod_1_Rsolnp$pars)
