
load("2015-2019/data/input.RData")

M1 = matrix(0, ncol = 2*n, nrow = N)
M2 = matrix(0, ncol = 2*n, nrow = N)
for(k in 1:N) {
  alpha = rep(0, n)
  beta = rep(0, n)
  alpha[i[k]] = 1
  beta[j[k]] = 1
  M1[k,] = c(alpha, beta)
  M2[k,] = c(beta, alpha)
}
M1 = cbind(M1, rep(1, N))
M2 = cbind(M2, rep(0, N))
colnames(M1) = c(paste0("alpha_", 1:n), paste0("beta_", 1:n), "gamma")
colnames(M2) = c(paste0("alpha_", 1:n), paste0("beta_", 1:n), "gamma")
M = rbind(M1, M2)
goals = c(x, y)

rm(k)
save.image("2015-2019/data/input_mod_0.RData")



