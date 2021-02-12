
library(dplyr)

load("data/gols.RData")
load("data/resultados.RData")

resultados = resultados %>%
  mutate(Id = 1:nrow(.))

gols = gols %>% 
  mutate(Minuto = ifelse(Tempo == "1º", Minuto, Minuto + 45)) %>%
  select(-Tempo, -Acréscimo) %>%
  inner_join(resultados)

# Preciso de de delta_00, ..., delta_22 e t_00, ..., t_22 para todas as partidas

ids_com_gol = unique(gols$Id)
indices = c("0-0", "1-0", "0-1", "1-1", "2-0", "0-2", "2-1", "1-2", "2-2") # vetor com os placares que estamos interessados
lst_delta = list()
lst_t = list()

# Partidas com gols
for(id in ids_com_gol) {
  gols_da_partida = gols %>%
    filter(Id == id)
  placar_mandante = 0
  placar_visitante = 0
  qtd_gols = nrow(gols_da_partida)
  for(row in 1:qtd_gols) {
    if(gols_da_partida$Time[row] == "Mandante") {
      placar_mandante[row+1] = placar_mandante[row] + 1
      placar_visitante[row+1] = placar_visitante[row]
    } else {
      placar_mandante[row+1] = placar_mandante[row]
      placar_visitante[row+1] = placar_visitante[row] + 1
    }
  }
  placares = paste(placar_mandante, placar_visitante, sep = "-")
  minutos = c(0, gols_da_partida$Minuto, 90)
  dif = diff(minutos)
  delta = rep(NA, 9)
  t = rep(0, 9)
  for(i in 1:9) {
    delta[i] = ifelse(indices[i] %in% placares[-length(placares)], 1, 0)
  }
  for(i in 1:length(dif)) {
    t[which(indices == placares[i])] = dif[i]
  }
  lst_delta[[id]] = delta
  lst_t[[id]] = t
}

# Partidas sem gols
ids_sem_gol = setdiff(1:nrow(resultados), ids_com_gol)
for(id in ids_sem_gol) {
  delta = rep(0, 9)
  t = c(90, rep(0, 8))
  lst_delta[[id]] = delta
  lst_t[[id]] = t
}

rates = cbind(resultados, do.call(rbind, lst_delta), do.call(rbind, lst_t))
names(rates)[10:27] = c("delta_00", "delta_10", "delta_01", "delta_11", "delta_20", 
                        "delta_02", "delta_21", "delta_12", "delta_22",
                        "t_00", "t_10", "t_01", "t_11", "t_20", 
                        "t_02", "t_21", "t_12", "t_22")

save(rates, file = "data/rates.RData")
