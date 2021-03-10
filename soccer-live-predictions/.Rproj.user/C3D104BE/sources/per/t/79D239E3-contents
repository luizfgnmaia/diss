
library(dplyr)

load("scrape/data/results.RData")
load("scrape/data/goals.RData")
load("scrape/data/reds.RData")

results = results %>%
  filter(Season == 2020)

goals = goals %>%
  filter(Season == 2020)

reds = reds %>%
  filter(Season == 2020)

#### Resultados
results$ind = 1:nrow(results)

copy_results = results %>%
  select(Season, Match, ind)

# U1, U2 (acréscimos)
U1 = results$Stoppage_Time_1
U2 = results$Stoppage_Time_2

# times, i, j, N, n
times = tibble(Id = 1:length(unique(results$Home_Team)), Time = sort(unique(results$Home_Team)))

tmp1 = times %>%
  rename(Home_Team = Time,
         i = Id)

tmp2 = times %>%
  rename(Away_Team = Time,
         j = Id)

results = results %>%
  inner_join(tmp1) %>%
  inner_join(tmp2) %>%
  rename(x = Score_Home,
         y = Score_Away) %>%
  mutate(k = 1:nrow(.)) %>%
  select(k, i, j, x, y)

i = results$i; j = results$j; x = results$x; y = results$y

N = nrow(results); n = nrow(times)

#### Gols
goals = goals %>%
  mutate(J = ifelse(Team == 2, 1, 0))
goals$Stoppage_Time[is.na(goals$Stoppage_Time)] = 0
goals$Minute = goals$Minute + goals$Stoppage_Time

goals = goals %>%
  left_join(copy_results)

# t1, t2, J1, J2, x1, x2, y1, y2, m1, m2, I1, I2
gols_primeiro_tempo = goals %>%
  filter(Half == 1)
gols_segundo_tempo = goals %>%
  filter(Half == 2)

t1 = list(); t2 = list(); J1 = list(); J2 = list()
x1 = list(); x2 = list(); y1 = list(); y2 = list()
m1 = NULL; m2 = NULL

for(k in 1:N) {
  primeiro_tempo = gols_primeiro_tempo %>%
    filter(ind == k)
  segundo_tempo = gols_segundo_tempo %>%
    filter(ind == k)
  t1[[k]] = primeiro_tempo$Minute
  t2[[k]] = segundo_tempo$Minute
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
        tmp_x1[(mandante$Minute[m]+1):(45+U1[k]+1)] = tmp_x1[mandante$Minute[m]]+1
      }
    }
    if(nrow(visitante > 0)) {
      for(m in 1:nrow(visitante)) {
        tmp_y1[(visitante$Minute[m]+1):(45+U1[k]+1)] = tmp_y1[visitante$Minute[m]]+1
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
        tmp_x2[(mandante$Minute[m]+1):(45+U2[k]+1)] = tmp_x2[mandante$Minute[m]]+1
      }
    }
    if(nrow(visitante > 0)) {
      for(m in 1:nrow(visitante)) {
        tmp_y2[(visitante$Minute[m]+1):(45+U2[k]+1)] = tmp_y2[visitante$Minute[m]]+1
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
  mutate(J = ifelse(Team == 2, 1, 0))
reds$Stoppage_Time[is.na(reds$Stoppage_Time)] = 0
reds$Minute = reds$Minute + reds$Stoppage_Time

reds = reds %>%
  inner_join(copy_results)

# t1s, t2s, J1s, J2s, x1s, x2s, y1s, y2s, m1s, m2s, I1s, I2s
reds_primeiro_tempo = reds %>%
  filter(Half == 1)
reds_segundo_tempo = reds %>%
  filter(Half == 2)

t1s = list(); t2s = list(); J1s = list(); J2s = list()
x1s = list(); x2s = list(); y1s = list(); y2s = list()
m1s = NULL; m2s = NULL

for(k in 1:N) {
  primeiro_tempo = reds_primeiro_tempo %>%
    filter(ind == k)
  segundo_tempo = reds_segundo_tempo %>%
    filter(ind == k)
  t1s[[k]] = primeiro_tempo$Minute
  t2s[[k]] = segundo_tempo$Minute
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
        tmp_x1[(mandante$Minute[m]+1):(45+U1[k]+1)] = tmp_x1[mandante$Minute[m]] + mandante %>% filter(Minute == mandante$Minute[m], Stoppage_Time == mandante$Stoppage_Time[m]) %>% nrow()
      }
    }
    if(nrow(visitante > 0)) {
      for(m in 1:nrow(visitante)) {
        tmp_y1[(visitante$Minute[m]+1):(45+U1[k]+1)] = tmp_y1[visitante$Minute[m]] + visitante %>% filter(Minute == visitante$Minute[m], Stoppage_Time == visitante$Stoppage_Time[m]) %>% nrow()
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
        tmp_x2[(mandante$Minute[m]+1):(45+U2[k]+1)] = tmp_x2[mandante$Minute[m]] + mandante %>% filter(Minute == mandante$Minute[m], Stoppage_Time == mandante$Stoppage_Time[m]) %>% nrow()
      }
    }
    if(nrow(visitante > 0)) {
      for(m in 1:nrow(visitante)) {
        tmp_y2[(visitante$Minute[m]+1):(45+U2[k]+1)] = tmp_y2[visitante$Minute[m]] + visitante %>% filter(Minute == visitante$Minute[m], Stoppage_Time == visitante$Stoppage_Time[m]) %>% nrow()
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

I1s = list(); I2s = list()
for(k in 1:N) {
  I1s[[k]] = c(0, t1s[[k]], 45+U1[k]) %>%
    unique() %>%
    sort()
  I2s[[k]] = c(0, t2s[[k]], 45+U2[k]) %>%
    unique() %>%
    sort()
}

# I1r e I2r (intervalos para os gols considerando que a taxa muda com um cartão vermelho)
I1r = list(); I2r = list()
for(k in 1:N) {
  I1r[[k]] = c(I1[[k]], I1s[[k]]) %>%
    unique() %>%
    sort()
  I2r[[k]] = c(I2[[k]], I2s[[k]]) %>%
    unique() %>%
    sort()
}

# H1, H2, A1, A2 (dummies para os gols)
H1 = list(); H2 = list(); A1 = list(); A2 = list()
for(k in 1:N) {
  if(length(t1[[k]]) > 0) {
    if(t1[[k]][length(t1[[k]])] == (45+U1[k])) {
      H1[[k]] = c(as.integer(!J1[[k]]))
      A1[[k]] = c(J1[[k]])
    } else {
      H1[[k]] = c(as.integer(!J1[[k]]), 0)
      A1[[k]] = c(J1[[k]], 0)
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
      A2[[k]] = c(J2[[k]], 0)
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

# H1r, H2r, A1r, A2r (dummies para os gols considerando que a taxa muda com um cartão vermelho)
H1r = list(); H2r = list(); A1r = list(); A2r = list()
for(k in 1:N) {
  tmp_H1 = NULL; tmp_A1 = NULL; tmp_H2 = NULL; tmp_A2 = NULL
  for(l in 2:(length(I1r[[k]]))) {
    tmp_H1[l-1] = x1[[k]][I1r[[k]][l]+1] - x1[[k]][I1r[[k]][l]]
    tmp_A1[l-1] = y1[[k]][I1r[[k]][l]+1] - y1[[k]][I1r[[k]][l]]
  }
  for(l in 2:(length(I2r[[k]]))) {
    tmp_H2[l-1] = x2[[k]][I2r[[k]][l]+1] - x2[[k]][I2r[[k]][l]]
    tmp_A2[l-1] = y2[[k]][I2r[[k]][l]+1] - y2[[k]][I2r[[k]][l]]
  }
  H1r[[k]] = tmp_H1
  A1r[[k]] = tmp_A1
  H2r[[k]] = tmp_H2
  A2r[[k]] = tmp_A2
}

H1r = unlist(H1r)
H2r = unlist(H2r)
A1r = unlist(A1r)
A2r = unlist(A2r)

# H1s, H2s, A1s, A2s (dummies para os cartões vermelhos) (se existirem dois cartões para um mesmo time no mesmo minuto, recebe valor 2 mas isso não aconteceu em 2020)
H1s = list(); H2s = list(); A1s = list(); A2s = list()
for(k in 1:N) {
  tmp_H1s = NULL; tmp_A1s = NULL; tmp_H2s = NULL; tmp_A2s = NULL
  for(l in 2:(length(I1s[[k]]))) {
    tmp_H1s[l-1] = x1s[[k]][I1s[[k]][l]+1] - x1s[[k]][I1s[[k]][l]]
    tmp_A1s[l-1] = y1s[[k]][I1s[[k]][l]+1] - y1s[[k]][I1s[[k]][l]]
  }
  for(l in 2:(length(I2s[[k]]))) {
    tmp_H2s[l-1] = x2s[[k]][I2s[[k]][l]+1] - x2s[[k]][I2s[[k]][l]]
    tmp_A2s[l-1] = y2s[[k]][I2s[[k]][l]+1] - y2s[[k]][I2s[[k]][l]]
  }
  H1s[[k]] = tmp_H1s
  A1s[[k]] = tmp_A1s
  H2s[[k]] = tmp_H2s
  A2s[[k]] = tmp_A2s
}

H1s = unlist(H1s)
H2s = unlist(H2s)
A1s = unlist(A1s)
A2s = unlist(A2s)

# g1, r1, g2, r2 (variáveis para os acréscimos)
g1 = unlist(lapply(t1, function(x) sum(x < 45))) 
r1 = unlist(lapply(t1s, function(x) sum(x < 45)))
g2 = unlist(lapply(t2, function(x) sum(x < 45))) 
r2 = unlist(lapply(t2s, function(x) sum(x < 45))) 

# c (variável da diferença de gols para o acréscimo do segundo tempo)
c = NULL
for(k in 1:N) {
  c[k] = as.integer(abs(x2[[k]][46] - y2[[k]][46]) <= 1)
}

times$Time = stringr::str_replace_all(times$Time, "\\s-\\s.*", "")
times$Time[1] = "Athletico-PR"
times$Time[2] = "Athletico-GO"
times$Time[3] = "Atlético-MG"

rm(list = setdiff(ls(), c("U1", "U2", "times", "i", "j", "N", "n", "x", "y",
                          "t1", "t2", "J1", "J2", "x1", "x2", "y1", "y2", "m1", "m2", "I1", "I2",
                          "t1s", "t2s", "J1s", "J2s", "x1s", "x2s", "y1s", "y2s", "m1s", "m2s",
                          "I1s", "I2s", "I1r", "I2r", "H1", "H2", "A1", "A2", "H1r", "H2r", "A1r", "A2r",
                          "H1s", "H2s", "A1s", "A2s", "g1", "r1", "g2", "r2", "c")))

save.image("2020/data/input.RData")