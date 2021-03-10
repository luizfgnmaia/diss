
# lambda_k(t) = alpha_i * beta_j * gamma * lambda_xy * e^{f(t)*csi_1}
# log(lambda_k(t)) = log(alpha_i) + log(beta_j) + log(gamma) + log(lambda_xy) + f(t)*csi_1

# f(t) = t

library(CVXR)

csi = Variable(1)
t1 = 1
t2 = 2

expr1 = (exp(t2*csi) - exp(t1*csi)) / csi # what we need
is_dcp(expr1)

expr2 = exp(t2*csi) - exp(t1*csi)
is_dcp(expr2)

?one_minus_pos
expr3 = one_minus_pos(exp(csi*(t1-t2)))
is_dcp(expr3)

expr4 = 1 - exp(csi*(t1-t2))
is_dcp(expr4) # ???

expr5 = log(1 - exp(csi*(t1-t2)))
is_dcp(expr5)
  
expr6 = t2*csi + log(1 - exp(csi*(t1-t2)))
is_dcp(expr6)
is_dcp(exp(expr6))

expr7 = t2*csi + log(1 - exp(csi*(t1-t2))) - log(csi) # log of what we need
is_dcp(expr7)
curvature(expr7)
curvature(-log(csi))

log_csi = Variable(1)
expr8 = t2*exp(log_csi) + log(1-exp(exp(log_csi)*(t1-t2))) - log_csi # log of what we need
is_dcp(expr8)

expr9 = log(1-exp(exp(log_csi)*(t1-t2)))
is_dcp(expr9)

