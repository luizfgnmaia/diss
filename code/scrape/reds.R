
load("data/reds_cbf_serie_a.RData")
load("data/reds_cbf_serie_b.RData")

reds = rbind(reds_cbf_serie_a, reds_cbf_serie_b) %>%
  mutate(Minuto = as.integer(Minuto),
         Acréscimo = as.integer(Acréscimo))

save(reds, file = "data/reds.RData")
