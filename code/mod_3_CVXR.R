
library(CVXR)

load("serie_a_2019.RData")

t0 = Sys.time()

alpha = Variable(19)
beta = Variable(20)
gamma = Variable(1)
lambda_xy = Variable(2)
mu_xy = Variable(2)
ro = Variable(2)
theta = vstack(alpha, beta, gamma, lambda_xy, mu_xy, ro)

log_lik = - t(delta_st) %*% exp(M1_mod3 %*% theta) - t(delta_st) %*% exp(M2_mod3 %*% theta) + t(H_st) %*% (M1_mod3 %*% theta) + t(A_st) %*% (M2_mod3 %*% theta)
objective = Maximize(log_lik)
problem = Problem(objective)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_3_CVXR = list(par = c(0, solution$getValue(alpha), solution$getValue(beta), solution$getValue(gamma),
                          solution$getValue(lambda_xy), solution$getValue(mu_xy), solution$getValue(ro)),
                  value = solution$value,
                  duration = duration)
save(mod_3_CVXR, file = "sol/mod_3_CVXR.RData")

load("sol/mod_3_Rsolnp.RData")
plot(exp(mod_3_CVXR$par), mod_3_Rsolnp$pars)
cor(exp(mod_3_CVXR$par), mod_3_Rsolnp$pars)

