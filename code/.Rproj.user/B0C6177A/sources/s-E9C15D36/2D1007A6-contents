
library(dplyr)
load("dados_serie_a_2019.RData")
load("dados_mod_1.RData")

score_index_3_1 = list()
for(k in 1:N) {
  tmp_score_index = NULL
  for(l in 1:(length(I1[[k]])-1)) {
    tmp_x = x1[[k]][I1[[k]][l]+1]
    tmp_y = y1[[k]][I1[[k]][l]+1]
    tmp_score_index[l] = ifelse(tmp_x == tmp_y, 1, 
                                ifelse(tmp_x - tmp_y >= 1, 2, 
                                       ifelse(tmp_x - tmp_y <= -1, 3,
                                             0)))
  } 
  score_index_3_1[[k]] = tmp_score_index
}

score_index_3_2 = list() # conferir depois esse aqui
for(k in 1:N) {
  tmp_score_index = NULL
  for(l in 1:(length(I2[[k]])-1)) {
    tmp_x = x2[[k]][I2[[k]][l]+1-45]
    tmp_y = y2[[k]][I2[[k]][l]+1-45]
    tmp_score_index[l] = ifelse(tmp_x == tmp_y, 1, 
                                ifelse(tmp_x - tmp_y >= 1, 2, 
                                       ifelse(tmp_x - tmp_y <= -1, 3,
                                              0)))
  } 
  score_index_3_2[[k]] = tmp_score_index
}

zero_1 = rep(0, L1)
zero_2 = rep(0, L2)

M_score_index_3_1 = unlist(score_index_3_1)
lambda_10_1 = rep(0, L1)
lambda_01_1 = rep(0, L1)
mu_10_1 = rep(0, L1)
mu_01_1 = rep(0, L1)
lambda_10_1[which(M_score_index_3_1 == 2)] = 1
lambda_01_1[which(M_score_index_3_1 == 3)] = 1
mu_10_1[which(M_score_index_3_1 == 2)] = 1
mu_01_1[which(M_score_index_3_1 == 3)] = 1

M_score_index_3_2 = unlist(score_index_3_2)
lambda_10_2 = rep(0, L2)
lambda_01_2 = rep(0, L2)
mu_10_2 = rep(0, L2)
mu_01_2 = rep(0, L2)
lambda_10_2[which(M_score_index_3_2 == 2)] = 1
lambda_01_2[which(M_score_index_3_2 == 3)] = 1
mu_10_2[which(M_score_index_3_2 == 2)] = 1
mu_01_2[which(M_score_index_3_2 == 3)] = 1

M1_lambda = cbind(M1_lambda, lambda_10_1, lambda_01_1, zero_1, zero_1)
colnames(M1_lambda)[41:44] = c("lambda_10", "lambda_01", "mu_10", "mu_01")
M2_lambda = cbind(M2_lambda, lambda_10_2, lambda_01_2, zero_2, zero_2)
colnames(M2_lambda)[41:44] = c("lambda_10", "lambda_01", "mu_10", "mu_01")

M1_mu = cbind(M1_mu, zero_1, zero_1, mu_10_1, mu_01_1)
colnames(M1_mu)[41:44] = c("lambda_10", "lambda_01", "mu_10", "mu_01")
M2_mu = cbind(M2_mu, zero_2, zero_2, mu_10_2, mu_01_2)
colnames(M2_mu)[41:44] = c("lambda_10", "lambda_01", "mu_10", "mu_01")

rm(list = setdiff(ls(), c("delta1", "delta2", "L1", "L2", "M1_lambda", "M1_mu", "M2_lambda", 
                          "M2_mu", "r1", "s1", "r2", "s2", "c"))) 

save.image("dados_mod_2.RData")


