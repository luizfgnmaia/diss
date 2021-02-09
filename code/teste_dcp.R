
library(CVXR)

t1 = 1 
t2 = 2
M = 3
csi = Variable(1)

expr1 = exp(t2*csi)
is_dcp(expr1)

expr2 = exp(t1*csi)
is_dcp(expr2)

expr3 = 1/csi
is_dcp(expr3)

is_dcp(expr1 - expr2)

expr4 = log(expr1 - expr2)
is_dcp(expr4)

expr5 = log_sum_exp(vstack(t2*csi, -t1*csi))
is_dcp(expr5)

expr6 = exp(expr5) - log(csi)
is_dcp(expr6)       

expr7 = exp(expr6)
is_dcp(expr7)

f_t_csi = exp(log_sum_exp(vstack(t2*csi, -t1*csi)) - log(csi))
is_dcp(f_t_csi)
curvature(f_t_csi)

theta = Variable(1)

expr8 = exp(M*theta) * f_t_csi
is_dcp(expr8)

expr9 = exp(M*theta + log_sum_exp(vstack(t2*csi, -t1*csi)) - log(csi))
is_dcp(expr9)

loglambda = M*theta + log_sum_exp(vstack(t2*csi, -t1*csi)) - log(csi)
lambda = exp(loglambda)

curvature(loglambda)
curvature(-exp(loglambda))
curvature(loglambda - exp(loglambda))