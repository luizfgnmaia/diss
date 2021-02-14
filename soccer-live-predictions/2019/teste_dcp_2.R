
library(CVXR)

t1 = 1
t2 = 2
csi = Variable(1)
expcsi = Variable(1)
logcsi = Variable(1)

expr1 = t2*csi + log(1-exp(csi*(t1-t2))) - log(csi)
is_dcp(expr1)
is_dcp(t2*csi)
is_dcp(log(1-exp(csi*(t1-t2))))
is_dcp(-log(csi))
curvature(t2*csi)
curvature(log(1-exp(csi*(t1-t2))))
curvature(-log(csi))

expr2 = t2*log(expcsi) + log(1-power(expcsi, (t1-t2))) - log(log(expcsi))
is_dcp(expr1)
is_dcp(t2*log(expcsi))
is_dcp(log(1-power(expcsi, (t1-t2))))
is_dcp(-log(log(expcsi)))
curvature(t2*log(expcsi))
curvature(log(1-power(expcsi, (t1-t2))))
curvature(-log(log(expcsi)))

expr3 = t2*exp(csi) + log(1-exp(exp(csi)*(t1-t2))) - csi
is_dcp(expr3)
is_dcp(t2*exp(csi))
is_dcp(log(1-exp(exp(csi)*(t1-t2))))
is_dcp(-csi)
curvature(t2*exp(csi))
curvature(log(1-exp(exp(csi)*(t1-t2))))
curvature(-csi)

