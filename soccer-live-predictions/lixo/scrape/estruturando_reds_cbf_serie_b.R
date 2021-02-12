
library(dplyr)
library(stringr)
library(stringdist)

load("data/scrape_cbf_serie_b.RData")
rm(list = setdiff(ls(), c("time_1", "time_2", "data", "placar_1", "placar_2")))
load("data/scrape_reds_cbf_serie_b.RData")
source("fix_date.R")

time_1 = time_1[-c(1:380)]
time_2 = time_2[-c(1:380)]
data = data[-c(1:380)]
placar_1 = placar_1[-c(1:380)]
placar_2 = placar_2[-c(1:380)]
time_1[which(time_1 == "America Fc - MG")] = "América - MG"
time_2[which(time_2 == "America Fc - MG")] = "América - MG"
time_1[which(time_1 == "Atletico - PR")] = "Athletico Paranaense - PR"
time_2[which(time_2 == "Atletico - PR")] = "Athletico Paranaense - PR"
time_1[which(time_1 == "Atlético - PR")] = "Athletico Paranaense - PR"
time_2[which(time_2 == "Atlético - PR")] = "Athletico Paranaense - PR"

partida = rep(1:380, 6)
ano = c(rep(2014, 380), rep(2015, 380), rep(2016, 380), rep(2017, 380), rep(2018, 380), rep(2019, 380))

data = fix_date(data)

reds = list()
for(jogo in 1:length(time_1)) {
  if(length(min[[jogo]]) > 0) {
    reds[[jogo]] = tibble(Ano = ano[jogo], 
                          Jogo = partida[jogo],
                          Data = data[jogo],
                          Time_1 = time_1[jogo], 
                          Placar_1 = placar_1[jogo], 
                          Placar_2 = placar_2[jogo], 
                          Time_2 = time_2[jogo], 
                          Time = time[[jogo]], 
                          Minuto = min[[jogo]], 
                          Tempo = tempo[[jogo]])    
  }
}

reds = do.call(rbind, reds)

acres = str_extract(reds$Minuto, "(?<=\\+)[0-9]*") %>%
  as.integer()

reds$Minuto[!is.na(acres)] = 45

tmp = reds$Tempo

reds = reds %>%
  select(-Tempo) 

reds$Acréscimo = acres
reds$Tempo = tmp

# tem que usar as siglas pra prever o time certo e depois usar o match pra garantir
fix_team <- function(time, time_1, time_2) {
  sigla_1 = str_extract(time_1, "(?<=\\s-\\s).*")
  sigla_2 = str_extract(time_2, "(?<=\\s-\\s).*")
  sigla = str_extract(time, "(?<=\\/).*")
  if(sigla == sigla_1 & sigla != sigla_2) {
    ind = 1 
  } else {
    if(sigla == sigla_2 & sigla != sigla_1) {
      ind = 2
    } else {
      ind = amatch(x = time, table = c(time_1, time_2), matchNA = FALSE, maxDist = 50)
    }
  }
  ifelse(ind == 1, "Mandante", "Visitante")
}

reds = reds %>%
  rowwise() %>%
  mutate(Time = fix_team(Time, Time_1, Time_2),
         Minuto = as.integer(Minuto),
         Tempo = paste0(Tempo, "º")) 

reds_cbf_serie_b = reds
save(reds_cbf_serie_b, file = "data/reds_cbf_serie_b.RData")

