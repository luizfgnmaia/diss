
load("serie_a_2019.RData")

goals = c(x, y)
alpha = as.factor(c(i, j))
beta = as.factor(c(j, i))
gamma = c(rep(1, N), rep(0, N))
# View(model.matrix(~ beta + alpha + gamma - 1))

mod_0_glm = glm(goals ~ beta + alpha + gamma - 1, family = poisson)
save(mod_0_glm, file = "sol/mod_0_glm.RData")





