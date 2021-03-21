
load("2020/data/input.RData")
load("2020/data/input_mod_r1.RData")

zero_1 = rep(0, L1)
zero_2 = rep(0, L2)

x_minus_y = M1_lambda[, "x"] - M1_lambda[, "y"]
ys_minus_xs = M1_lambda[, "ys"] - M1_lambda[, "xs"]
M1_lambda = M1_lambda[,-c(43:50)]
M1_lambda = cbind(M1_lambda, x_minus_y, zero_1, ys_minus_xs, zero_1)
colnames(M1_lambda)[43:46] = c("x-y", "0", "ys-xs", "0")

y_minus_x = M1_mu[, "y"] - M1_mu[, "x"]
xs_minus_ys = M1_mu[, "xs"] - M1_mu[, "ys"]
M1_mu = M1_mu[,-c(43:50)]
M1_mu = cbind(M1_mu, zero_1, y_minus_x, zero_1, xs_minus_ys)
colnames(M1_mu)[43:46] = c("0", "y-x", "0", "xs-ys")

x_minus_y = M2_lambda[, "x"] - M2_lambda[, "y"]
ys_minus_xs = M2_lambda[, "ys"] - M2_lambda[, "xs"]
M2_lambda = M2_lambda[,-c(43:50)]
M2_lambda = cbind(M2_lambda, x_minus_y, zero_2, ys_minus_xs, zero_2)
colnames(M2_lambda)[43:46] = c("x-y", "0", "ys-xs", "0")

y_minus_x = M2_mu[, "y"] - M2_mu[, "x"]
xs_minus_ys = M2_mu[, "xs"] - M2_mu[, "ys"]
M2_mu = M2_mu[,-c(43:50)]
M2_mu = cbind(M2_mu, zero_2, y_minus_x, zero_2, xs_minus_ys)
colnames(M2_mu)[43:46] = c("0", "y-x", "0", "xs-ys")

rm(list = setdiff(ls(), c("delta1", "delta2", "L1", "L2", "M1_lambda", "M1_mu", "M2_lambda", 
                          "M2_mu", "delta1s", "delta2s", "int_reds_1", "int_reds_2")))

save.image("2020/data/input_mod_r2.RData")
