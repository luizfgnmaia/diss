
# lambda_k(t) = alpha_i * beta_j * gamma * lambda_xy * e^{f(t)*csi_1}
# log(lambda_k(t)) = log(alpha_i) + log(beta_j) + log(gamma) + log(lambda_xy) + f(t)*csi_1

# f(t) = log(t+1)

library(CVXR)

csi = Variable(1)
t1 = 1
t2 = 2

expr1 = (exp((csi+1)*log(t2+1)) - exp((csi+1)*log(t1+1))) / (csi+1) # what we need
is_dcp(expr1)

expr2 = exp((csi+1)*log(t2+1)) - exp((csi+1)*log(t1+1))
is_dcp(expr2)

expr3 = exp((csi+1)*log(t2+1))
expr4 = exp((csi+1)*log(t1+1))

sign(expr3)
sign(expr4)
sign(expr3 - expr4)

