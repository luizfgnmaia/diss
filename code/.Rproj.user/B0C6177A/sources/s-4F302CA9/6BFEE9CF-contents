
library(dplyr)
load("dados_serie_a_2019.RData")

# delta1, delta2, L1, L2, M1_lambda, M1_mu, M2_lambda, M2_mu
diff1 = list()
diff2 = list()
for(k in 1:N) {
  diff1[[k]] = diff(I1[[k]])
  diff2[[k]] = diff(I2[[k]])
}
delta1 = unlist(diff1)
delta2 = unlist(diff2)

L1 = length(delta1)
L2 = length(delta2)

M1_lambda = matrix(0, ncol = 40, nrow = L1) 
row = 0
for(k in 1:N) {
  alpha = rep(0, 20)
  beta = rep(0, 20)
  alpha[i[k]] = 1
  beta[j[k]] = 1
  tmp = c(alpha, beta)
  for(l in 1:length(diff1[[k]])) {
    M1_lambda[(row+l),] = tmp
  }
  row = row + length(diff1[[k]])
}
gamma = rep(1, L1)
M1_lambda = cbind(M1_lambda, gamma)
M1_lambda = M1_lambda[,-1]
colnames(M1_lambda) = c(paste0("alpha_", 2:20), paste0("beta", 1:20), "gamma")

M2_lambda = matrix(0, ncol = 40, nrow = L2) 
row = 0
for(k in 1:N) {
  alpha = rep(0, 20)
  beta = rep(0, 20)
  alpha[i[k]] = 1
  beta[j[k]] = 1
  tmp = c(alpha, beta)
  for(l in 1:length(diff2[[k]])) {
    M2_lambda[(row+l),] = tmp
  }
  row = row + length(diff2[[k]])
}
gamma = rep(1, L2)
M2_lambda = cbind(M2_lambda, gamma)
M2_lambda = M2_lambda[,-1]
colnames(M2_lambda) = c(paste0("alpha_", 2:20), paste0("beta", 1:20), "gamma")

M1_mu = matrix(0, ncol = 40, nrow = L1)
row = 0
for(k in 1:N) {
  alpha = rep(0, 20)
  beta = rep(0, 20)
  alpha[j[k]] = 1
  beta[i[k]] = 1
  tmp = c(alpha, beta)
  for(l in 1:(length(diff1[[k]]))) {
    M1_mu[(row+l),] = tmp
  }
  row = row + length(diff1[[k]])
}
gamma = rep(0, L1)
M1_mu = cbind(M1_mu, gamma)
M1_mu = M1_mu[,-1]
colnames(M1_mu) = c(paste0("alpha_", 2:20), paste0("beta", 1:20), "gamma")

M2_mu = matrix(0, ncol = 40, nrow = L2)
row = 0
for(k in 1:N) {
  alpha = rep(0, 20)
  beta = rep(0, 20)
  alpha[j[k]] = 1
  beta[i[k]] = 1
  tmp = c(alpha, beta)
  for(l in 1:(length(diff2[[k]]))) {
    M2_mu[(row+l),] = tmp
  }
  row = row + length(diff2[[k]])
}
gamma = rep(0, L2)
M2_mu = cbind(M2_mu, gamma)
M2_mu = M2_mu[,-1]
colnames(M2_mu) = c(paste0("alpha_", 2:20), paste0("beta", 1:20), "gamma")

# r1, s1, r2, s2
r1 = unlist(lapply(t1, function(x) length(x < 45)))
s1 = unlist(lapply(t1s, function(x) length(x < 45)))
r2 = unlist(lapply(t2, function(x) length(x < 90)))
s2 = unlist(lapply(t2s, function(x) length(x < 90)))

# c
c = NULL
for(k in 1:N) {
  c[k] = as.integer(abs(x2[[k]][46] - y2[[k]][46]) <= 1)
}

rm(list = setdiff(ls(), c("delta1", "delta2", "L1", "L2", "M1_lambda", "M1_mu", "M2_lambda", 
                          "M2_mu", "r1", "s1", "r2", "s2", "c"))) 

save.image("dados_mod_1.RData")