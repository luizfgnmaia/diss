
load("2018/data/input.RData")

diff1 = list()
diff2 = list()
for(k in 1:N) {
  diff1[[k]] = diff(I1r[[k]])
  diff2[[k]] = diff(I2r[[k]])
}
delta1 = unlist(diff1)
delta2 = unlist(diff2)

L1 = length(delta1)
L2 = length(delta2)

M1_lambda = matrix(0, ncol = 2*n, nrow = L1) 
row = 0
for(k in 1:N) {
  alpha = rep(0, n)
  beta = rep(0, n)
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
colnames(M1_lambda) = c(paste0("alpha_", 1:n), paste0("beta_", 1:n), "gamma")

M2_lambda = matrix(0, ncol = 2*n, nrow = L2) 
row = 0
for(k in 1:N) {
  alpha = rep(0, n)
  beta = rep(0, n)
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
colnames(M2_lambda) = c(paste0("alpha_", 1:n), paste0("beta_", 1:n), "gamma")

M1_mu = matrix(0, ncol = 2*n, nrow = L1)
row = 0
for(k in 1:N) {
  alpha = rep(0, n)
  beta = rep(0, n)
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
colnames(M1_mu) = c(paste0("alpha_", 1:n), paste0("beta_", 1:n), "gamma")

M2_mu = matrix(0, ncol = 2*n, nrow = L2)
row = 0
for(k in 1:N) {
  alpha = rep(0, n)
  beta = rep(0, n)
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
colnames(M2_mu) = c(paste0("alpha_", 1:n), paste0("beta_", 1:n), "gamma")

eta = rep(0, L1)
M1_lambda = cbind(M1_lambda, eta)

eta = rep(1, L2)
M2_lambda = cbind(M2_lambda, eta)

eta = rep(0, L1)
M1_mu = cbind(M1_mu, eta)

eta = rep(1, L2)
M2_mu = cbind(M2_mu, eta)

vet_x1 = list()
vet_y1 = list()
for(k in 1:N) {
  tmp_x1 = NULL
  tmp_y1 = NULL
  for(l in 1:(length(I1r[[k]])-1)) {
    tmp_x1[l] = x1[[k]][I1r[[k]][l]+1]
    tmp_y1[l] = y1[[k]][I1r[[k]][l]+1]
  } 
  vet_x1[[k]] = tmp_x1
  vet_y1[[k]] = tmp_y1
}
vet_x1 = unlist(vet_x1)
vet_y1 = unlist(vet_y1)

vet_x2 = list()
vet_y2 = list()
for(k in 1:N) {
  tmp_x2 = NULL
  tmp_y2 = NULL
  for(l in 1:(length(I2r[[k]])-1)) {
    tmp_x2[l] = x2[[k]][I2r[[k]][l]+1]
    tmp_y2[l] = y2[[k]][I2r[[k]][l]+1]
  } 
  vet_x2[[k]] = tmp_x2
  vet_y2[[k]] = tmp_y2
}
vet_x2 = unlist(vet_x2)
vet_y2 = unlist(vet_y2)

zero_1 = rep(0, L1)
zero_2 = rep(0, L2)

M1_lambda = cbind(M1_lambda, vet_x1, vet_y1, zero_1, zero_1)
colnames(M1_lambda)[(2*n+3):(ncol(M1_lambda))] = c("x", "y", "0", "0")

M1_mu = cbind(M1_mu, zero_1, zero_1, vet_x1, vet_y1)
colnames(M1_mu)[(2*n+3):(ncol(M1_mu))] = c("0", "0", "x", "y")

M2_lambda = cbind(M2_lambda, vet_x2, vet_y2, zero_2, zero_2)
colnames(M2_lambda)[(2*n+3):(ncol(M2_lambda))] = c("x", "y", "0", "0")

M2_mu = cbind(M2_mu, zero_2, zero_2, vet_x2, vet_y2)
colnames(M2_mu)[(2*n+3):(ncol(M2_mu))] = c("0", "0", "x", "y")

exp_H1 = list(); exp_A1 = list(); exp_H2 = list(); exp_A2 = list()
for(k in 1:N) {
  tmp_exp_H1 = NULL; tmp_exp_A1 = NULL; tmp_exp_H2 = NULL; tmp_exp_A2 = NULL
  for(l in 2:(length(I1r[[k]]))) { 
    tmp_exp_H1[l-1] = x1s[[k]][I1r[[k]][l]] # penúltimo minuto do intervalo
    tmp_exp_A1[l-1] = y1s[[k]][I1r[[k]][l]]
  }
  for(l in 2:(length(I2r[[k]]))) { 
    tmp_exp_H2[l-1] = x2s[[k]][I2r[[k]][l]] 
    tmp_exp_A2[l-1] = y2s[[k]][I2r[[k]][l]]
  }
  exp_H1[[k]] = tmp_exp_H1
  exp_A1[[k]] = tmp_exp_A1
  exp_H2[[k]] = tmp_exp_H2
  exp_A2[[k]] = tmp_exp_A2
}

exp_H1 = unlist(exp_H1)
exp_A1 = unlist(exp_A1)
exp_H2 = unlist(exp_H2)
exp_A2 = unlist(exp_A2)

M1_lambda = cbind(M1_lambda, exp_H1, exp_A1, zero_1, zero_1)
colnames(M1_lambda)[(2*n+7):(ncol(M1_lambda))] = c("xs", "ys", "0", "0")

M1_mu = cbind(M1_mu, zero_1, zero_1, exp_H1, exp_A1)
colnames(M1_mu)[(2*n+7):(ncol(M1_mu))] = c("0", "0", "xs", "ys")

M2_lambda = cbind(M2_lambda, exp_H2, exp_A2, zero_2, zero_2)
colnames(M2_lambda)[(2*n+7):(ncol(M2_lambda))] = c("xs", "ys", "0", "0")

M2_mu = cbind(M2_mu, zero_2, zero_2, exp_H2, exp_A2)
colnames(M2_mu)[(2*n+7):(ncol(M2_mu))] = c("0", "0", "xs", "ys")

diff1s = list()
diff2s = list()
for(k in 1:N) {
  diff1s[[k]] = diff(I1s[[k]])
  diff2s[[k]] = diff(I2s[[k]])
}
delta1s = unlist(diff1s)
delta2s = unlist(diff2s)

for(k in 1:N) {
  I2s[[k]] = I2s[[k]] + 45
}

int_reds_1 = list(); int_reds_2 = list();
for(k in 1:N) {
  tmp_int_reds_1 = NULL
  for(l in 1:(length(I1s[[k]])-1)) {
    t1 = I1s[[k]][l]
    t2 = I1s[[k]][l+1]
    tmp_int_reds_1[l] = 1/2*(t2^2 - t1^2)
  }
  int_reds_1[[k]] = tmp_int_reds_1
}
for(k in 1:N) {
  tmp_int_reds_2 = NULL
  for(l in 1:(length(I2s[[k]])-1)) {
    t1 = I2s[[k]][l]
    t2 = I2s[[k]][l+1]
    tmp_int_reds_2[l] = 1/2*(t2^2 - t1^2)
  }
  int_reds_2[[k]] = tmp_int_reds_2
}
int_reds_1 = unlist(int_reds_1)
int_reds_2 = unlist(int_reds_2)

rm(list = setdiff(ls(), c("delta1", "delta2", "L1", "L2", "M1_lambda", "M1_mu", "M2_lambda", 
                          "M2_mu", "delta1s", "delta2s", "int_reds_1", "int_reds_2")))

save.image("2018/data/input_mod_1.RData")

