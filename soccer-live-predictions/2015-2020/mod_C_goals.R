
library(CVXR)

load("2015-2020/data/input.RData")
load("2015-2020/data/input_mod_12.RData")

M1_lambda = M1_lambda[,1:(2*n+4)] 
M2_lambda = M2_lambda[,1:(2*n+4)] 
M1_mu = M1_mu[,1:(2*n+4)] 
M2_mu = M2_mu[,1:(2*n+4)] 

t0 = Sys.time()

alpha = Variable(n)
beta = Variable(n)
gamma = Variable(1)
tau = Variable(1)
omega = Variable(1)
theta = vstack(alpha, beta, gamma, tau, omega, omega)

loglambda1 = log(delta1) + M1_lambda %*% theta
logmu1 = log(delta1) + M1_mu %*% theta
loglambda2 = log(delta2) + M2_lambda %*% theta
logmu2 = log(delta2) + M2_mu %*% theta

log_lik_goals = sum_entries(
  - exp(loglambda1) - exp(logmu1) +
    H1r*loglambda1 + A1r*logmu1) +
  sum_entries(
    - exp(loglambda2) - exp(logmu2) +
      + H2r*loglambda2 + A2r*logmu2)

objective = Maximize(log_lik_goals)
constraints = list(sum(alpha) - sum(beta) == 0)
problem = Problem(objective, constraints)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_C_goals = list(alpha = as.vector(c(solution$getValue(alpha))),
                    beta = as.vector(solution$getValue(beta)),
                    gamma = as.vector(solution$getValue(gamma)),
                    tau = as.vector(solution$getValue(tau)),
                    omega = as.vector(solution$getValue(omega)),
                    loglik = solution$value,
                    duration = duration)
names(mod_C_goals$alpha) = times$Time
names(mod_C_goals$beta) = times$Time
names(mod_C_goals$omega) = "goal"

save(mod_C_goals, file = "2015-2020/data/mod_C_goals.RData")

