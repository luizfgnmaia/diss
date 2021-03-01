
library(dplyr)
library(stringr)

load("scrape/data/scrape_reds.RData")
load("scrape/data/results.RData")

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
minuto[which(tempo == "INT")] = 1
tempo[which(tempo == "INT")] = "2T" # Intervalos estão como 1 minuto do segundo tempo
tempo = as.integer(str_replace_all(tempo, "T", ""))

time = split2[,2] %>%
  str_extract_all("(?<=\\s\\-\\s).*") %>%
  unlist()

numero = split2[,2] %>%
  str_sub(start = 1, end = 2) %>%
  str_squish()

time2 = NULL
for(i in 1:length(time)) {
  time2[i] = ifelse(time[i] == mandante[i], 1,
                    ifelse(time[i] == visitante[i], 2,
                           "Erro"))
}

results = results %>%
  select(-Stoppage_Time_1, -Stoppage_Time_2)

tib = tibble(Minute = minuto, Half = tempo, Team = time2, Number = numero, Match = jogo, Season = ano)

reds = inner_join(results, tib)

primeiro_caractere = substr(minuto, start = 1, stop = 1)
acres = minuto
acres[which(primeiro_caractere != "+")] = NA
acres = str_replace_all(acres, "\\+", "")

reds$Minute[which(!is.na(acres))] = 45
reds$Stoppage_Time = acres
reds$Minute[which(reds$Half == "PJ")] = NA

reds = reds %>%
  mutate(Number = as.integer(Number)) %>%
  filter(!is.na(Number)) %>%
  filter(!is.na(Minute)) %>%
  filter(!is.na(Half)) %>%
  select(-Number) %>%
  mutate(Minute = as.integer(Minute),
         Stoppage_Time = as.integer(Stoppage_Time)) %>%
  arrange(Season, Match, Half, Minute, Stoppage_Time)

deletar = c(14, 72, 78, 94, 102, 
            112, 168,
            202, 241, 264,
            285, 294, 296, 308, 323, 337,
            363, 418,
            509, 540)

reds = reds[-deletar,]

r1 = tibble(Season = 2017, Match = 378, Date = "2017-12-03", 
            Home_Team = "Vitória - BA", Score_Home = 1, Score_Away = 2, 
            Away_Team = "Flamengo - RJ", Minute = 45, Half = 2, Team = 1, Stoppage_Time = 4)

r2 = tibble(Season = 2018, Match = 141, Date = "2018-07-25", 
            Home_Team = "Atlético - MG", Score_Home = 2, Score_Away = 0, 
            Away_Team = "Paraná - PR", Minute = 35, Half = 2, Team = 1, Stoppage_Time = NA)

r3 = tibble(Season = 2020, Match = 160, Date = "2020-10-16", 
            Home_Team = "Goiás - GO", Score_Home = 1, Score_Away = 1, 
            Away_Team = "Bahia - BA", Minute = 29, Half = 2, Team = 2, Stoppage_Time = NA)

reds = rbind(reds, r1, r2, r3) %>%
  arrange(Season, Match, Half, Minute, Stoppage_Time)

save(reds, file = "scrape/data/reds.RData")

