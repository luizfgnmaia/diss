
library(CVXR)

t1 = 1
t2 = 2
csi = Variable(1)
# csi = Variable(1, pos = TRUE)

# f(t) = log(t+1)
expr1 = exp((csi+1)*log(t2+1))
expr2 = exp((csi+1)*log(t1+1))
is_dcp(expr1)

sign(expr1)
sign(expr2)
sign(expr1 - expr2)


is_dcp(expr1 - expr2)
is_dcp(log(expr1 - expr2))








# f(t) = t
expr1 = exp(t2*csi)
expr2 = exp(t1*csi)
is_dcp(expr1)

sign(expr1)
sign(expr2)
sign(expr1 - expr2)

is_dcp(expr1 - expr2)
is_dcp(log(expr1 - expr2))



expr3 = one_minus_pos(x = expr2/expr1)
is_dcp(expr3)
sign(expr3)



csi = Variable(1, pos = TRUE)

expr1 = exp(t2*csi)
is_dcp(expr1)
curvature(expr1)

expr2 = 1-exp(csi*(t1-t2))
is_dcp(expr2)
curvature(expr2)

expr3 = t2*csi
is_dcp(expr3)
curvature(expr3)

expr4 = log(expr2)
is_dcp(expr4)
curvature(expr4)

expr5 = t2*csi + log(1-exp(csi*(t1-t2)))
is_dcp(expr5)
curvature(expr5)

expr6 = t2*csi + log(1-exp(csi*(t1-t2))) - log(csi) # log do que eu preciso
is_dcp(expr6)
curvature(expr6)

logcsi = Variable(1)
expr7 = t2*exp(logcsi) + log(1-exp(exp(logcsi)*(t1-t2))) - logcsi # log do que eu preciso
is_dcp(expr7)
curvature(expr7)

expr8 = exp(logcsi)*(t1-t2)
is_dcp(expr8)
curvature(expr8)
sign(expr8)

expr9 = 1-exp(logcsi)*(t1-t2)
is_dcp(expr9)
curvature(expr9)

expr10 = log(expr9)
is_dcp(expr10)

expr11 = one_minus_pos(exp(logcsi)*(t1-t2))
is_dcp(expr11) # ??? não faz sentido comparando com expr9
curvature(expr11)
sign(expr11)

expr12 = CVXR:::DiffPos(exp(t2*csi), exp(t1*csi))
sign(expr12)
curvature(expr12)
is_dcp(expr12)

expr13 = CVXR:::DiffPos(1, exp(csi*(t1-t2)))
sign(expr13)
curvature(expr13)
is_dcp(expr13)






