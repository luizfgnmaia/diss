
library(dplyr)
library(stringr)

load("data/scrape_cbf_serie_a.RData")
source("fix_date.R")

partida = rep(1:380, 7)
ano = c(rep(2013, 380), rep(2014, 380), rep(2015, 380), rep(2016, 380), rep(2017, 380), rep(2018, 380), rep(2019, 380))

data = fix_date(data)

# Todos os gols dos times mandantes
gols_casa = list()
for(jogo in 1:length(time_1)) {
  min = str_extract_all(gols_1[jogo], "(?<= )[0-9+]*(?=')") %>% 
    unlist()
  acres = str_extract(min, "(?<=\\+)[0-9]*") %>%
    unlist() %>%
    as.integer()
  min = str_extract(min, "[0-9]*") %>%
    as.integer()
  tempo = str_extract_all(gols_1[jogo], "(?<=\\()[1-2]*(?=ºT\\))") %>%
    unlist() %>%
    as.integer()
  gols_casa[[jogo]] = tibble(Ano = ano[jogo], 
                             Jogo = partida[jogo], 
                             Data = data[jogo],
                             Time_1 = time_1[jogo], 
                             Placar_1 = placar_1[jogo], 
                             Placar_2 = placar_2[jogo], 
                             Time_2 = time_2[jogo], 
                             Time = 1, 
                             Minuto = min, 
                             Acréscimo = acres, 
                             Tempo = tempo)
}

# Todos os gols dos times visitantes
gols_fora = list()
for(jogo in 1:length(time_1)) {
  min = str_extract_all(gols_2[jogo], "(?<= )[0-9+]*(?=')") %>% 
    unlist()
  acres = str_extract(min, "(?<=\\+)[0-9]*") %>%
    unlist() %>%
    as.integer()
  min = str_extract(min, "[0-9]*") %>%
    as.integer()
  tempo = str_extract_all(gols_2[jogo], "(?<=\\()[1-2]*(?=ºT\\))") %>%
    unlist() %>%
    as.integer()
  gols_fora[[jogo]] = tibble(Ano = ano[jogo], 
                             Jogo = partida[jogo],
                             Data = data[jogo],
                             Time_1 = time_1[jogo], 
                             Placar_1 = placar_1[jogo], 
                             Placar_2 = placar_2[jogo], 
                             Time_2 = time_2[jogo], 
                             Time = 2, 
                             Minuto = min, 
                             Acréscimo = acres, 
                             Tempo = tempo)
}

gols_casa = do.call(rbind, gols_casa)
gols_fora = do.call(rbind, gols_fora)

gols = rbind(gols_casa, gols_fora) %>%
  arrange(Ano, Jogo, Time_1, Time_2, Tempo, Minuto, Acréscimo) %>%
  mutate(Ano = as.integer(Ano),
         Jogo = as.integer(Jogo),
         Placar_1 = as.integer(Placar_1),
         Placar_2 = as.integer(Placar_2),
         Acréscimo = as.integer(Acréscimo),
         Time = recode(Time, "1" = "Mandante", "2" = "Visitante"),
         Tempo = recode(Tempo, "1" = "1º", "2" = "2º"))

gols$Time_1[which(gols$Time_1 == "America Fc - MG")] = "América - MG"
gols$Time_2[which(gols$Time_2 == "America Fc - MG")] = "América - MG"
gols$Time_1[which(gols$Time_1 == "Atletico - PR")] = "Athletico Paranaense - PR"
gols$Time_2[which(gols$Time_2 == "Atletico - PR")] = "Athletico Paranaense - PR"
gols$Time_1[which(gols$Time_1 == "Atlético - PR")] = "Athletico Paranaense - PR"
gols$Time_2[which(gols$Time_2 == "Atlético - PR")] = "Athletico Paranaense - PR"

gols$Minuto[290] = 45L # tinha um erro
gols$Acréscimo[290] = 2L

gols_cbf_serie_a = gols
save(gols_cbf_serie_a, file = "data/gols_cbf_serie_a.RData")
