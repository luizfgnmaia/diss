
library(dplyr)

load("data/resultados_cbf_serie_a.RData")
load("data/resultados_cbf_serie_b.RData")
load("data/scrape_st_cbf_serie_a.RData")
load("data/scrape_st_cbf_serie_b.RData")

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

first_half_serie_a = c(rep(NA, 380), first_half_serie_a)
second_half_serie_a = c(rep(NA, 380), second_half_serie_a)
first_half_serie_b = c(rep(NA, 380), first_half_serie_b)
second_half_serie_b = c(rep(NA, 380), second_half_serie_b)

Acréscimos_1 = c(first_half_serie_a, first_half_serie_b)
Acréscimos_2 = c(second_half_serie_a, second_half_serie_b)

resultados$Acréscimos_1 = Acréscimos_1
resultados$Acréscimos_2 = Acréscimos_2

save(resultados, file = "data/resultados.RData")