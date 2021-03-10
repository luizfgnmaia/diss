
library(CVXR)

load("2019/data/input.RData")

t0 = Sys.time()

goals = c(x, y)
alpha = as.factor(c(i, j))
beta = as.factor(c(j, i))
gamma = c(rep(1, N), rep(0, N))

# https://stackoverflow.com/questions/4560459/all-levels-of-a-factor-in-a-model-matrix-in-r
df = data.frame(alpha, beta, gamma)
dmy = caret::dummyVars(" ~ .", data = df) 
M = as.matrix(data.frame(predict(dmy, newdata = df)))

alpha = Variable(20)
beta = Variable(20)
gamma = Variable(1)
theta = vstack(alpha, beta, gamma)

log_lik = sum_entries(goals * M %*% theta - exp(M %*% theta))

objective = Maximize(log_lik)
constraints = list(sum(alpha) - sum(beta) == 0)
problem = Problem(objective, constraints)
set.seed(1)
solution = solve(problem, solver = "MOSEK")

duration = Sys.time() - t0

mod_0 = list(alpha = as.vector(c(solution$getValue(alpha))),
             beta = as.vector(solution$getValue(beta)),
             gamma = as.vector(solution$getValue(gamma)),
             value = solution$value,
             duration = duration)
names(mod_0$alpha) = times$Time
names(mod_0$beta) = times$Time

save(mod_0, file = "2019/data/mod_0.RData")




