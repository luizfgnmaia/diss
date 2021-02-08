
library(dplyr)

load("dados_serie_a_2019.RData")
load("dados_mod_3.RData")

# Para cada intervalo, preciso do t1 e do t2

t1_1 = list()
t2_1 = list()
t1_2 = list()
t2_2 = list()

for(k in 1:N) {
  len = length(I1[[k]])
  t1_1[[k]] = I1[[k]][1:(len-1)]
  t2_1[[k]] = I1[[k]][2:len]
}

for(k in 1:N) {
  len = length(I2[[k]])
  t1_2[[k]] = I2[[k]][1:(len-1)]
  t2_2[[k]] = I2[[k]][2:len]
}

t1_1 = unlist(t1_1)
t2_1 = unlist(t2_1)
t1_2 = unlist(t1_2)
t2_2 = unlist(t2_2)

rm(list = setdiff(ls(), c("delta1", "delta2", "L1", "L2", "M1_lambda", "M1_mu", "M2_lambda", 
                          "M2_mu", "g1", "r1", "g2", "r2", "c", "t1_1", "t1_2", "t2_1", "t2_2"))) 

save.image("dados_mod_4.RData")