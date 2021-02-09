
library(CVXR)

t1 = 1 
t2 = 2
M = 3
csi = Variable(1)

# expr0 = exp((t2+1)^(csi+1)) # erro, não existe ^ no CVXR
# is_dcp(expr0)

expr1 = exp((csi+1)*log(t2+1))
is_dcp(expr1)

expr2 = exp((csi+1)*log(t1+1))
is_dcp(expr2)

expr3 = 1/(csi+1)
is_dcp(expr3)

is_dcp(expr1 - expr2)

expr4 = log(expr1 - expr2)
is_dcp(expr4)

expr5 = log_sum_exp(vstack((csi+1)*log(t2+1), -(csi+1)*log(t1+1))) # tá errado, não é o que preciso
is_dcp(expr5)

expr6 = exp(expr5) - log(csi+1)
is_dcp(expr6)       

expr7 = exp(expr6)
is_dcp(expr7)

f_t_csi = exp(log_sum_exp(vstack((csi+1)*log(t2+1), -(csi+1)*log(t1+1))) - log(csi+1))
is_dcp(f_t_csi)
curvature(f_t_csi)

theta = Variable(1)

expr8 = exp(M*theta) * f_t_csi
is_dcp(expr8)

expr9 = exp(log_sum_exp(vstack((csi+1)*log(t2+1), -(csi+1)*log(t1+1))) - log(csi+1) - log(csi))
is_dcp(expr9)

loglambda = M*theta + log_sum_exp(vstack((csi+1)*log(t2+1), -(csi+1)*log(t1+1))) - log(csi+1) - log(csi)

curvature(loglambda)
curvature(-exp(loglambda))
curvature(loglambda - exp(loglambda))
