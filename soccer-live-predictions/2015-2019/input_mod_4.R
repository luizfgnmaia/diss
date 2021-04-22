
load("2015-2019/data/input.RData")
load("2015-2019/data/input_mod_2.RData")

M1_lambda = cbind(M1_lambda, M1_lambda[,"x-y"]^2, 0)
colnames(M1_lambda)[(ncol(M1_lambda)-1):ncol(M1_lambda)] = c("(x-y)^2", "0")

M2_lambda = cbind(M2_lambda, M2_lambda[,"x-y"]^2, 0)
colnames(M2_lambda)[(ncol(M2_lambda)-1):ncol(M2_lambda)] = c("(x-y)^2", "0")

M1_mu = cbind(M1_mu, 0, M1_mu[,"y-x"]^2)
colnames(M1_mu)[(ncol(M1_mu)-1):ncol(M1_mu)] = c("0", "(y-x)^2")

M2_mu = cbind(M2_mu, 0, M2_mu[,"y-x"]^2)
colnames(M2_mu)[(ncol(M2_mu)-1):ncol(M2_mu)] = c("0", "(y-x)^2")

save.image("2015-2019/data/input_mod_4.RData")