
library(dplyr)

load("2015-2020/data/input.RData")
load("2015-2020/data/input_mod_2.RData")
load("transfermarkt/data/results.RData")

res = results %>%
  select(Season, Date, Match, Dif_Value, Dif_Log_Value)

match_dates = match_dates %>%
  left_join(res)

dif_value_1 = NULL
dif_value_2 = NULL
for(i in 1:nrow(match_dates)) {
  dif_value_1[match_dates$Lines1[i][[1]]] = match_dates$Dif_Value[i]/10^6 # <---
  dif_value_2[match_dates$Lines2[i][[1]]] = match_dates$Dif_Value[i]/10^6 # <---
}

M1_lambda = cbind(M1_lambda, dif_value_1)
colnames(M1_lambda)[ncol(M1_lambda)] = "dif_value"

M1_mu = cbind(M1_mu, -dif_value_1)
colnames(M1_mu)[ncol(M1_mu)] = "dif_value"

M2_lambda = cbind(M2_lambda, dif_value_2)
colnames(M2_lambda)[ncol(M2_lambda)] = "dif_value"

M2_mu = cbind(M2_mu, -dif_value_2)
colnames(M2_mu)[ncol(M2_mu)] = "dif_value"

save.image("2015-2020/data/input_mod_11.RData")