
load("2020/data/input.RData")
load("2020/data/input_mod_2.RData") # não preciso dos dados do 3

vet_x1 = list()
vet_y1 = list()
for(k in 1:N) {
  tmp_x1 = NULL
  tmp_y1 = NULL
  for(l in 1:(length(I1[[k]])-1)) {
    tmp_x1[l] = x1[[k]][I1[[k]][l]+1]
    tmp_y1[l] = y1[[k]][I1[[k]][l]+1]
  } 
  vet_x1[[k]] = tmp_x1
  vet_y1[[k]] = tmp_y1
}
vet_x1 = unlist(vet_x1)
vet_y1 = unlist(vet_y1)

vet_x2 = list()
vet_y2 = list()
for(k in 1:N) {
  tmp_x2 = NULL
  tmp_y2 = NULL
  for(l in 1:(length(I2[[k]])-1)) {
    tmp_x2[l] = x2[[k]][I2[[k]][l]+1]
    tmp_y2[l] = y2[[k]][I2[[k]][l]+1]
  } 
  vet_x2[[k]] = tmp_x2
  vet_y2[[k]] = tmp_y2
}
vet_x2 = unlist(vet_x2)
vet_y2 = unlist(vet_y2)

zero_1 = rep(0, L1)
zero_2 = rep(0, L2)

M1_lambda = cbind(M1_lambda, vet_x1, vet_y1, zero_1, zero_1)
colnames(M1_lambda)[(2*n+3):(ncol(M1_lambda))] = c("x", "y", "0", "0")

M1_mu = cbind(M1_mu, zero_1, zero_1, vet_x1, vet_y1)
colnames(M1_mu)[(2*n+3):(ncol(M1_mu))] = c("0", "0", "x", "y")

M2_lambda = cbind(M2_lambda, vet_x2, vet_y2, zero_2, zero_2)
colnames(M2_lambda)[(2*n+3):(ncol(M2_lambda))] = c("x", "y", "0", "0")

M2_mu = cbind(M2_mu, zero_2, zero_2, vet_x2, vet_y2)
colnames(M2_mu)[(2*n+3):(ncol(M2_mu))] = c("0", "0", "x", "y")

rm(list = setdiff(ls(), c("delta1", "delta2", "L1", "L2", "M1_lambda", "M1_mu", "M2_lambda", 
                          "M2_mu", "g1", "r1", "g2", "r2", "c")))

save.image("2020/data/input_mod_4.RData")
