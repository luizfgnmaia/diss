
library(dplyr)

load("data/scrape_cbf_serie_b.RData")
source("fix_date.R")

partida = rep(1:380, 7)
ano = c(rep(2013, 380), rep(2014, 380), rep(2015, 380), rep(2016, 380), rep(2017, 380), rep(2018, 380), rep(2019, 380))

data = fix_date(data)

resultados = tibble(Ano = ano,
                    Jogo = partida,
                    Data = data,
                    Time_1 = time_1,
                    Placar_1 = placar_1,
                    Placar_2 = placar_2,
                    Time_2 = time_2) %>%
  mutate(Ano = as.integer(Ano),
         Placar_1 = as.integer(Placar_1),
         Placar_2 = as.integer(Placar_2))

resultados$Time_1[which(resultados$Time_1 == "America Fc - MG")] = "América - MG"
resultados$Time_2[which(resultados$Time_2 == "America Fc - MG")] = "América - MG"

resultados_cbf_serie_b = resultados
save(resultados_cbf_serie_b, file = "data/resultados_cbf_serie_b.RData")

