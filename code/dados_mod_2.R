
load("dados_serie_a_2019.RData")
load("dados_mod_1.RData")

eta = rep(0, L1)
M1_lambda = cbind(M1_lambda, eta)

eta = rep(1, L2)
M2_lambda = cbind(M2_lambda, eta)

eta = rep(0, L1)
M1_mu = cbind(M1_mu, eta)

eta = rep(1, L2)
M2_mu = cbind(M2_mu, eta)

rm(list = setdiff(ls(), c("delta1", "delta2", "L1", "L2", "M1_lambda", "M1_mu", "M2_lambda", 
                          "M2_mu", "g1", "r1", "g2", "r2", "c"))) 

save.image("dados_mod_2.RData")