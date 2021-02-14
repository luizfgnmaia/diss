
library(dplyr)

load("data/resultados.RData")
load("data/gols.RData")
load("data/reds.RData")

#### Resultados
resultados = resultados %>%
  filter(Campeonato == "Campeonato Brasileiro Série A",
         Ano >= 2014) %>%
  filter(!is.na(Placar_1))
resultados$ind = 1:nrow(resultados)

copy_resultados = resultados %>%
  select(Campeonato, Ano, Jogo, ind)

# U1, U2
U1 = resultados$Acréscimos_1
U2 = resultados$Acréscimos_2

# times, i, j, N, n
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

i = resultados$i; j = resultados$j; x = resultados$x; y = resultados$y

N = nrow(resultados); n = nrow(times)

#### Gols
gols = gols %>%
  filter(Campeonato == "Campeonato Brasileiro Série A",
         Ano >= 2014) %>%
  mutate(J = ifelse(Time == "Visitante", 1, 0))
gols$Acréscimo[is.na(gols$Acréscimo)] = 0
gols$Minuto = gols$Minuto + gols$Acréscimo

gols = gols %>%
  left_join(copy_resultados)

# t1, t2, J1, J2, x1, x2, y1, y2, m1, m2, I1, I2
gols_primeiro_tempo = gols %>%
  filter(Tempo == "1º")
gols_segundo_tempo = gols %>%
  filter(Tempo == "2º")

t1 = list(); t2 = list(); J1 = list(); J2 = list()
x1 = list(); x2 = list(); y1 = list(); y2 = list()
m1 = NULL; m2 = NULL

for(k in 1:N) {
  primeiro_tempo = gols_primeiro_tempo %>%
    filter(ind == k)
  segundo_tempo = gols_segundo_tempo %>%
    filter(ind == k)
  t1[[k]] = primeiro_tempo$Minuto
  t2[[k]] = segundo_tempo$Minuto
  J1[[k]] = primeiro_tempo$J
  J2[[k]] = segundo_tempo$J
  m1[k] = nrow(primeiro_tempo)
  m2[k] = nrow(segundo_tempo)
  
  if(nrow(primeiro_tempo) > 0) {
    tmp_x1 = rep(0, 45+U1[k]+1) # primeira entrada é o minuto 0
    tmp_y1 = rep(0, 45+U1[k]+1)
    mandante = primeiro_tempo %>%
      filter(J == 0)
    visitante = primeiro_tempo %>%
      filter(J == 1)
    if(nrow(mandante > 0)) {
      for(m in 1:nrow(mandante)) {
        tmp_x1[(mandante$Minuto[m]+1):(45+U1[k]+1)] = tmp_x1[mandante$Minuto[m]]+1
      }
    }
    if(nrow(visitante > 0)) {
      for(m in 1:nrow(visitante)) {
        tmp_y1[(visitante$Minuto[m]+1):(45+U1[k]+1)] = tmp_y1[visitante$Minuto[m]]+1
      }
    }
    x1[[k]] = tmp_x1
    y1[[k]] = tmp_y1
  } else {
    x1[[k]] = rep(0, (45+U1[k]+1))
    y1[[k]] = rep(0, (45+U1[k]+1))
  }  
  
  if(nrow(segundo_tempo) > 0) {
    tmp_x2 = rep(0, 45+U2[k]+1) # primeira entrada é o minuto 45
    tmp_y2 = rep(0, 45+U2[k]+1)
    mandante = segundo_tempo %>%
      filter(J == 0)
    visitante = segundo_tempo %>%
      filter(J == 1)
    if(nrow(mandante > 0)) {
      for(m in 1:nrow(mandante)) {
        tmp_x2[(mandante$Minuto[m]+1):(45+U2[k]+1)] = tmp_x2[mandante$Minuto[m]]+1
      }
    }
    if(nrow(visitante > 0)) {
      for(m in 1:nrow(visitante)) {
        tmp_y2[(visitante$Minuto[m]+1):(45+U2[k]+1)] = tmp_y2[visitante$Minuto[m]]+1
      }
    }
    x2[[k]] = tmp_x2
    y2[[k]] = tmp_y2
  } else {
    x2[[k]] = rep(0, (45+U2[k]+1))
    y2[[k]] = rep(0, (45+U2[k]+1))
  }  
}

for(k in 1:N) {
  x2[[k]] = x2[[k]] + x1[[k]][length(x1[[k]])]
  y2[[k]] = y2[[k]] + y1[[k]][length(y1[[k]])]
}

I1 = list(); I2 = list()
for(k in 1:N) {
  I1[[k]] = c(0, t1[[k]], 45+U1[k]) %>%
    unique() %>%
    sort()
  I2[[k]] = c(0, t2[[k]], 45+U2[k]) %>%
    unique() %>%
    sort()
}

#### Reds
reds = reds %>%
  filter(Campeonato == "Campeonato Brasileiro Série A",
         Ano >= 2014) %>%
  mutate(J = ifelse(Time == "Visitante", 1, 0))
reds$Acréscimo[is.na(reds$Acréscimo)] = 0
reds$Minuto[which(reds$Tempo == "2º")] = reds$Minuto[which(reds$Tempo == "2º")] + 45
reds$Minuto = reds$Minuto + reds$Acréscimo

reds = reds %>%
  inner_join(copy_resultados)

# t1s, t2s, J1s, J2s, x1s, x2s, y1s, y2s, m1s, m2s
reds_primeiro_tempo = reds %>%
  filter(Tempo == "1º")
reds_segundo_tempo = reds %>%
  filter(Tempo == "2º")

t1s = list(); t2s = list(); J1s = list(); J2s = list()
x1s = list(); x2s = list(); y1s = list(); y2s = list()
m1s = NULL; m2s = NULL

for(k in 1:N) {
  primeiro_tempo = reds_primeiro_tempo %>%
    filter(ind == k)
  segundo_tempo = reds_segundo_tempo %>%
    filter(ind == k)
  t1s[[k]] = primeiro_tempo$Minuto
  t2s[[k]] = segundo_tempo$Minuto
  J1s[[k]] = primeiro_tempo$J
  J2s[[k]] = segundo_tempo$J
  m1s[k] = nrow(primeiro_tempo)
  m2s[k] = nrow(segundo_tempo)
  
  if(nrow(primeiro_tempo) > 0) {
    tmp_x1 = rep(0, 45+U1[k]+1) # primeira entrada é o minuto 0
    tmp_y1 = rep(0, 45+U1[k]+1)
    mandante = primeiro_tempo %>%
      filter(J == 0)
    visitante = primeiro_tempo %>%
      filter(J == 1)
    if(nrow(mandante > 0)) {
      for(m in 1:nrow(mandante)) {
        tmp_x1[(mandante$Minuto[m]+1):(45+U1[k]+1)] = tmp_x1[mandante$Minuto[m]]+1
      }
    }
    if(nrow(visitante > 0)) {
      for(m in 1:nrow(visitante)) {
        tmp_y1[(visitante$Minuto[m]+1):(45+U1[k]+1)] = tmp_y1[visitante$Minuto[m]]+1
      }
    }
    x1s[[k]] = tmp_x1
    y1s[[k]] = tmp_y1
  } else {
    x1s[[k]] = rep(0, (45+U1[k]+1))
    y1s[[k]] = rep(0, (45+U1[k]+1))
  }  
  
  if(nrow(segundo_tempo) > 0) {
    tmp_x2 = rep(0, 45+U2[k]+1) # primeira entrada é o minuto 45
    tmp_y2 = rep(0, 45+U2[k]+1)
    mandante = segundo_tempo %>%
      filter(J == 0)
    visitante = segundo_tempo %>%
      filter(J == 1)
    if(nrow(mandante > 0)) {
      for(m in 1:nrow(mandante)) {
        tmp_x2[(mandante$Minuto[m]+1):(45+U2[k]+1)] = tmp_x2[mandante$Minuto[m]]+1
      }
    }
    if(nrow(visitante > 0)) {
      for(m in 1:nrow(visitante)) {
        tmp_y2[(visitante$Minuto[m]+1):(45+U2[k]+1)] = tmp_y2[visitante$Minuto[m]]+1
      }
    }
    x2s[[k]] = tmp_x2
    y2s[[k]] = tmp_y2
  } else {
    x2s[[k]] = rep(0, (45+U2[k]+1))
    y2s[[k]] = rep(0, (45+U2[k]+1))
  }  
}

for(k in 1:N) {
  x2s[[k]] = x2s[[k]] + x1s[[k]][length(x1s[[k]])]
  y2s[[k]] = y2s[[k]] + y1s[[k]][length(y1s[[k]])]
}

# H1, H2, A1, A2
H1 = list(); H2 = list(); A1 = list(); A2 = list()
for(k in 1:N) {
  if(length(t1[[k]]) > 0) {
    if(t1[[k]][length(t1[[k]])] == (45+U1[k])) {
      H1[[k]] = c(as.integer(!J1[[k]]))
      A1[[k]] = c(J1[[k]])
    } else {
      H1[[k]] = c(as.integer(!J1[[k]]), 0)
      A1[[k]] = A1[[k]] = c(J1[[k]], 0)
    }
  } else {
    H1[[k]] = 0
    A1[[k]] = 0
  }
  if(length(t2[[k]]) > 0) {
    if(t2[[k]][length(t2[[k]])] == (45+U2[k])) {
      H2[[k]] = c(as.integer(!J2[[k]]))
      A2[[k]] = c(J2[[k]])
    } else {
      H2[[k]] = c(as.integer(!J2[[k]]), 0)
      A2[[k]] = A2[[k]] = c(J2[[k]], 0)
    }
  } else {
    H2[[k]] = 0
    A2[[k]] = 0
  }
}
H1 = unlist(H1)
H2 = unlist(H2)
A1 = unlist(A1)
A2 = unlist(A2)

times$Time = stringr::str_replace_all(times$Time, "\\s-\\s.*", "")
times$Time[1] = "América-MG"
times$Time[2] = "Athletico-PR"
times$Time[3] = "Atlético-GO"
times$Time[4] = "Atlético-MG"

rm(list = setdiff(ls(), c("U1", "U2", "times", "i", "j", "N", "n", "x", "y",
                          "t1", "t2", "J1", "J2", "x1", "x2", "y1", "y2", "m1", "m2", "I1", "I2",
                          "t1s", "t2s", "J1s", "J2s", "x1s", "x2s", "y1s", "y2s", "m1s", "m2s",
                          "H1", "H2", "A1", "A2")))

save.image("2019/data/input.RData")