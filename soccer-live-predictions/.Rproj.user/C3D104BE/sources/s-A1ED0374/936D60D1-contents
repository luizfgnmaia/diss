
load("2018/data/input.RData")
load("2018/data/input_mod_1.RData")

zero_1 = rep(0, L1)
zero_2 = rep(0, L2)

x_minus_y = M1_lambda[, "x"] - M1_lambda[, "y"]
ys_minus_xs = M1_lambda[, "ys"] - M1_lambda[, "xs"]
M1_lambda = M1_lambda[,-c((2*n+3):(ncol(M1_lambda)))]
M1_lambda = cbind(M1_lambda, x_minus_y, zero_1, ys_minus_xs, zero_1)
colnames(M1_lambda)[(2*n+3):(ncol(M1_lambda))] = c("x-y", "0", "ys-xs", "0")

y_minus_x = M1_mu[, "y"] - M1_mu[, "x"]
xs_minus_ys = M1_mu[, "xs"] - M1_mu[, "ys"]
M1_mu = M1_mu[,-c((2*n+3):(ncol(M1_mu)))]
M1_mu = cbind(M1_mu, zero_1, y_minus_x, zero_1, xs_minus_ys)
colnames(M1_mu)[(2*n+3):(ncol(M1_mu))] = c("0", "y-x", "0", "xs-ys")

x_minus_y = M2_lambda[, "x"] - M2_lambda[, "y"]
ys_minus_xs = M2_lambda[, "ys"] - M2_lambda[, "xs"]
M2_lambda = M2_lambda[,-c((2*n+3):(ncol(M2_lambda)))]
M2_lambda = cbind(M2_lambda, x_minus_y, zero_2, ys_minus_xs, zero_2)
colnames(M2_lambda)[(2*n+3):(ncol(M2_lambda))] = c("x-y", "0", "ys-xs", "0")

y_minus_x = M2_mu[, "y"] - M2_mu[, "x"]
xs_minus_ys = M2_mu[, "xs"] - M2_mu[, "ys"]
M2_mu = M2_mu[,-c((2*n+3):(ncol(M2_mu)))]
M2_mu = cbind(M2_mu, zero_2, y_minus_x, zero_2, xs_minus_ys)
colnames(M2_mu)[(2*n+3):(ncol(M2_mu))] = c("0", "y-x", "0", "xs-ys")

save.image("2018/data/input_mod_2.RData")
