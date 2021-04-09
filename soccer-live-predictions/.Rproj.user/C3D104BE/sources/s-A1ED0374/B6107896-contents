
library(CVXR)
library(caret)

load("2020/data/input.RData")
load("2020/data/input_mod_0.RData")

t0 = Sys.time()

alpha = Variable(20)
beta = Variable(20)
gamma = Variable(1)
theta = vstack(alpha, beta, gamma)

log_lik = sum_entries(goals * M %*% theta - exp(M %*% theta))

objective = Maximize(log_lik)
constraints = list(sum(alpha) - sum(beta) == 0)
problem = Problem(objective, constraints)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_0 = list(alpha = as.vector(c(solution$getValue(alpha))),
             beta = as.vector(solution$getValue(beta)),
             gamma = as.vector(solution$getValue(gamma)),
             value = solution$value,
             duration = duration)
names(mod_0$alpha) = times$Time
names(mod_0$beta) = times$Time

save(mod_0, file = "2020/data/mod_0.RData")




