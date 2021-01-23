
library(dplyr)
library(stringr)

load("data/scrape_reds_cbf_serie_b.RData")
load("data/resultados.RData")

jogo = unlist(lst_jogo)
confronto = unlist(lst_confronto)
ano = unlist(lst_ano)
red = unlist(lst_red)

mandante = str_extract(confronto, ".*(?=\\sX\\s)") %>%
  str_replace_all(" / ", "/")
visitante = str_extract(confronto, "(?<=\\sX\\s).*") %>%
  str_replace_all(" / ", "/")

split = str_split_fixed(red, " ", 2)

minuto = split[,1] %>%
  str_replace_all(":00", "")

split2 = split[,2] %>%
  str_split_fixed(" ", 2)

tempo = split2[,1]
tempo[which(!tempo %in% c("1T", "2T", "PJ", "INT"))] = "PJ"
minuto[which(tempo == "INT")] = 0
tempo[which(tempo == "INT")] = "2T" # Intervalos estão como 0 minuto do segundo tempo
tempo = str_replace_all(tempo, "T", "º")

time = split2[,2] %>%
  str_extract_all("(?<=\\s\\-\\s).*") %>%
  unlist()

numero = split2[,2] %>%
  str_sub(start = 1, end = 2) %>%
  str_squish()

time2 = NULL
for(i in 1:length(time)) {
  time2[i] = ifelse(time[i] == mandante[i], "Mandante",
                    ifelse(time[i] == visitante[i], "Visitante",
                           "Erro"))
}

# tibble(mandante, visitante, time, time2, ano) %>% View() # Todos os erros tão em 2014 porque o nome dos times tá completo

tmp = readxl::read_excel("time_manual.xlsx", sheet = 2)
time2[1:99] = tmp$V2

resultados = resultados %>%
  filter(Campeonato == "Campeonato Brasileiro Série B") %>%
  select(Campeonato, Ano, Jogo, Data, Time_1, Placar_1, Placar_2, Time_2)

tib = tibble(Minuto = minuto, Tempo = tempo, Time = time2, Número = numero, Jogo = jogo, Ano = ano)

reds_cbf_serie_b = inner_join(resultados, tib)

primeiro_caractere = substr(minuto, start = 1, stop = 1)
acres = minuto
acres[which(primeiro_caractere != "+")] = NA
acres = str_replace_all(acres, "\\+", "")

reds_cbf_serie_b$Minuto[which(!is.na(acres))] = 45
reds_cbf_serie_b$Acréscimo = acres
reds_cbf_serie_b$Minuto[which(reds_cbf_serie_b$Tempo == "PJ")] = NA

reds_cbf_serie_b = reds_cbf_serie_b %>%
  filter(!is.na(Minuto)) %>% # tirando os PJ
  mutate(Número = as.integer(Número)) %>%
  filter(!is.na(Número)) %>%
  select(-Número)

save(reds_cbf_serie_b, file = "data/reds_cbf_serie_b.RData")