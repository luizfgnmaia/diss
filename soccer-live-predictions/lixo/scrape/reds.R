
library(dplyr)

load("data/reds_cbf_serie_a.RData")
load("data/reds_cbf_serie_b.RData")

reds_cbf_serie_a$Campeonato = "Campeonato Brasileiro Série A"
reds_cbf_serie_b$Campeonato = "Campeonato Brasileiro Série B"

reds = rbind(reds_cbf_serie_a, reds_cbf_serie_b) %>%
  ungroup()

reds = reds[,c(ncol(reds), 1:(ncol(reds)-1))]

save(reds, file = "data/reds.RData")
