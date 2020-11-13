
library(dplyr)

load("data/resultados.RData")
load("data/gols.RData")

resultados = resultados %>%
  na.omit() %>%
  filter(Ano >= 2014,
         Campeonato == "Campeonato Brasileiro Série A") %>%
  arrange(Data) %>%
  mutate(Jogo_k = 1:nrow(.))

tmp_k = resultados %>%
  select(Jogo, Campeonato, Ano, Jogo_k)

times = tibble(Id = 1:length(unique(resultados$Time_1)), Time = sort(unique(resultados$Time_1)))

tmp1 = times %>%
  rename(Time_1 = Time,
         i = Id)

tmp2 = times %>%
  rename(Time_2 = Time,
         j = Id)

resultados = resultados %>%
  inner_join(tmp1) %>%
  inner_join(tmp2) %>%
  rename(x = Placar_1,
         y = Placar_2) %>%
  mutate(k = 1:nrow(.)) %>%
  select(k, i, j, x, y)

i = resultados$i; j = resultados$j

N = nrow(resultados); n = nrow(times)

gols = gols %>%
  filter(Ano >= 2014,
         Campeonato == "Campeonato Brasileiro Série A") %>%
  mutate(Minuto = ifelse(Tempo == "1º", Minuto, Minuto + 45),
         t = Minuto/90,
         J = ifelse(Time == "Visitante", 1, 0)) %>%
  left_join(tmp_k) %>%
  select(-Jogo) %>%
  mutate(Jogo = Jogo_k) %>%
  arrange(Jogo)

lst_t = list()
lst_J = list()
lst_x = list()
lst_y = list()
for(k in 1:N) { # otimizar essa parte?
  jogo = gols %>%
    filter(Jogo == k)
  if(nrow(jogo) > 0) {
    lst_t[[k]] = jogo$t
    lst_J[[k]] = jogo$J
    tmp_x = rep(0, 91) # primeira entrada é o minuto 0
    tmp_y = rep(0, 91)
    mandante = jogo %>%
      filter(J == 0)
    visitante = jogo %>%
      filter(J == 1)
    if(nrow(mandante > 0)) {
      for(m in 1:nrow(mandante)) {
        tmp_x[(mandante$Minuto[m]+1):91] = tmp_x[mandante$Minuto[m]]+1
      }
    }
    if(nrow(visitante > 0)) {
      for(m in 1:nrow(visitante)) {
        tmp_y[(visitante$Minuto[m]+1):91] = tmp_y[visitante$Minuto[m]]+1
      }
    }
    lst_x[[k]] = tmp_x
    lst_y[[k]] = tmp_y
  } else {
    lst_t[[k]] = NA
    lst_J[[k]] = NA
    lst_x[[k]] = rep(0, 91)
    lst_y[[k]] = rep(0, 91)
  }
}

rm(list = setdiff(ls(), c("lst_J", "lst_t", "lst_x", "lst_y", "times", "i", "j", "n", "N")))
save.image("datasets/serie_a_14_19.RData")
