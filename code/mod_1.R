
library(dplyr)
library(CVXR)

load("dados_serie_a_2019.RData")
load("dados_mod_1.RData")

# delta1 = delta1/100
# delta2 = delta2/100

t0 = Sys.time()

alpha = Variable(19)
beta = Variable(20)
gamma = Variable(1)
theta = vstack(alpha, beta, gamma)

tau = Variable(2)
phi = Variable(2)
kappa = Variable(1)
pi1 = tau[1] + phi[1] * r1
pi2 = tau[2] + phi[2] * r2 + c * kappa

log_lik = -t(delta1)%*%exp(M1_lambda%*%theta) -t(delta1)%*%exp(M1_mu%*%theta) -t(delta2)%*%exp(M2_lambda%*%theta) -t(delta2)%*%exp(M2_mu%*%theta) +
  t(H1)%*%M1_lambda%*%theta +t(A1)%*%M1_mu%*%theta + t(H2)%*%M2_lambda%*%theta + t(A2)%*%M2_mu%*%theta +
  t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum_entries(pi1) - sum_entries(pi2)

objective = Maximize(log_lik)
problem = Problem(objective)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_1 = list(alpha = as.vector(c(0, solution$getValue(alpha))),
             beta = as.vector(solution$getValue(beta)),
             gamma = as.vector(solution$getValue(gamma)),
             tau = as.vector(solution$getValue(tau)),
             phi = as.vector(solution$getValue(phi)),
             kappa = as.vector(solution$getValue(kappa)),
             value = solution$value,
             duration = duration)
names(mod_1$alpha) = times$Time
names(mod_1$beta) = times$Time

save(mod_1, file = "mod_1.RData")

# library(ggplot2)
# 
# load("dixon robinson/sol/mod_1_CVXR.RData")
# 
# tibble(new = mod_1$par$val[1:41], old = mod_1_CVXR$par, par = c(rep("alpha", 20), rep("beta", 20), "gamma")) %>%
#   ggplot(aes(x = new, y = old, col = par)) +
#   geom_point(size = 2) +
#   theme_bw()

