
library(dplyr)

load("data/resultados_cbf_serie_a.RData")
load("data/resultados_cbf_serie_b.RData")

resultados_cbf_serie_a$Campeonato = "Campeonato Brasileiro Série A"
resultados_cbf_serie_b$Campeonato = "Campeonato Brasileiro Série B"

resultados = rbind(resultados_cbf_serie_a, resultados_cbf_serie_b)

resultados = resultados[,c(ncol(resultados), 1:(ncol(resultados)-1))]

resultados$Time_1[which(resultados$Time_1 == "Criciuma - SC")] = "Criciúma - SC"
resultados$Time_2[which(resultados$Time_2 == "Criciuma - SC")] = "Criciúma - SC"

resultados$Placar_1[5110] = NA
resultados$Placar_2[5110] = NA
resultados$Placar_1[3047] = NA
resultados$Placar_2[3047] = NA

save(resultados, file = "data/resultados.RData")