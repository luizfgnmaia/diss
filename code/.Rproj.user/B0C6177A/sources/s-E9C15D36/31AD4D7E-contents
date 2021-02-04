
library(dplyr)
load("dados_serie_a_2019.RData")
load("dados_mod_2.RData")

diff_quad1 = list()
diff_quad2 = list()
for(k in 1:N) {
  diff_quad1[[k]] = diff(I1[[k]]^2)
  diff_quad2[[k]] = diff(I2[[k]]^2)
}
delta_quad1 = unlist(diff_quad1)
delta_quad2 = unlist(diff_quad2)

rm(list = setdiff(ls(), c("delta1", "delta2", "L1", "L2", "M1_lambda", "M1_mu", "M2_lambda", 
                          "M2_mu", "r1", "s1", "r2", "s2", "c", "delta_quad1", "delta_quad2"))) 

save.image("dados_mod_3.RData")