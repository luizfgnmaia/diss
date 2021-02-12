
load("dados_serie_a_2019.RData")

goals = c(x, y)
alpha = as.factor(c(i, j))
beta = as.factor(c(j, i))
gamma = c(rep(1, N), rep(0, N))
# View(model.matrix(~ beta + alpha + gamma - 1))

solution = glm(goals ~ beta + alpha + gamma - 1, family = poisson)

mod_0 = list(alpha = c(0, solution$coefficients[21:39]),
             beta = solution$coefficients[1:20],
             gamma = solution$coefficients[40])
names(mod_0$alpha) = times$Time
names(mod_0$beta) = times$Time

save(mod_0, file = "mod_0.RData")


