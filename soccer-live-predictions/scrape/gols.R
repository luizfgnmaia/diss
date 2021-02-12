
library(dplyr)

load("data/gols_cbf_serie_a.RData")
load("data/gols_cbf_serie_b.RData")

gols_cbf_serie_a$Campeonato = "Campeonato Brasileiro Série A"
gols_cbf_serie_b$Campeonato = "Campeonato Brasileiro Série B"

gols = rbind(gols_cbf_serie_a, gols_cbf_serie_b)

gols = gols[,c(ncol(gols), 1:(ncol(gols)-1))]

gols$Time_1[which(gols$Time_1 == "Criciuma - SC")] = "Criciúma - SC"
gols$Time_2[which(gols$Time_2 == "Criciuma - SC")] = "Criciúma - SC"

r1 = tibble(Campeonato = "Campeonato Brasileiro Série A", Ano = 2019, Jogo = 371,
            Data = "2019-12-08", Time_1 = "Internacional - RS", Placar_1 = 2, Placar_2 = 1,
            Time_2 = "Atlético - MG", Time = "Visitante", Minuto = 5, Acréscimo = NA, Tempo = "1º")

r2 = tibble(Campeonato = "Campeonato Brasileiro Série A", Ano = 2019, Jogo = 371,
            Data = "2019-12-08", Time_1 = "Internacional - RS", Placar_1 = 2, Placar_2 = 1,
            Time_2 = "Atlético - MG", Time = "Mandante", Minuto = 41, Acréscimo = NA, Tempo = "2º")

r3 = tibble(Campeonato = "Campeonato Brasileiro Série A", Ano = 2019, Jogo = 371,
            Data = "2019-12-08", Time_1 = "Internacional - RS", Placar_1 = 2, Placar_2 = 1,
            Time_2 = "Atlético - MG", Time = "Mandante", Minuto = 45, Acréscimo = 7, Tempo = "2º")

r4 = tibble(Campeonato = "Campeonato Brasileiro Série A", Ano = 2016, Jogo = 292,
            Data = "2016-10-13", Time_1 = "Fluminense - RJ", Placar_1 = 1, Placar_2 = 2, 
            Time_2 = "Flamengo - RJ", Time = "Visitante", Minuto = 12, Acréscimo = NA, Tempo = "1º")

r5 = tibble(Campeonato = "Campeonato Brasileiro Série A", Ano = 2016, Jogo = 292,
            Data = "2016-10-13", Time_1 = "Fluminense - RJ", Placar_1 = 1, Placar_2 = 2, 
            Time_2 = "Flamengo - RJ", Time = "Mandante", Minuto = 1, Acréscimo = NA, Tempo = "2º")

r6 = tibble(Campeonato = "Campeonato Brasileiro Série A", Ano = 2016, Jogo = 292,
            Data = "2016-10-13", Time_1 = "Fluminense - RJ", Placar_1 = 1, Placar_2 = 2,
            Time_2 = "Flamengo - RJ", Time = "Visitante", Minuto = 8, Acréscimo = NA, Tempo = "2º")

gols = rbind(gols, r1, r2, r3, r4, r5, r6) %>%
  arrange(Data, Tempo, Minuto)

save(gols, file = "data/gols.RData")
