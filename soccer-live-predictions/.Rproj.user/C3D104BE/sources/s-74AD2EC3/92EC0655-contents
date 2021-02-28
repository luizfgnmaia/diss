
library(dplyr)
library(stringr)

load("scrape/data/scrape.RData")
source("scrape/fix_date.R")

partida = rep(1:380, 6)
ano = c(rep(2015, 380), rep(2016, 380), rep(2017, 380), rep(2018, 380), rep(2019, 380), rep(2020, 380))

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
  gols_casa[[jogo]] = tibble(Season = ano[jogo], 
                              Match = partida[jogo], 
                              Date = data[jogo],
                              Home_Team = time_1[jogo], 
                              Score_Home = placar_1[jogo], 
                              Score_Away = placar_2[jogo], 
                              Away_Team = time_2[jogo], 
                              Team = 1, 
                              Minute = min, 
                              Stoppage_Time = acres, 
                              Half = tempo)
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
  gols_fora[[jogo]] = tibble(Season = ano[jogo], 
                              Match = partida[jogo],
                              Date = data[jogo],
                              Home_Team = time_1[jogo], 
                              Score_Home = placar_1[jogo], 
                              Score_Away = placar_2[jogo], 
                              Away_Team = time_2[jogo], 
                              Team = 2, 
                              Minute = min, 
                              Stoppage_Time = acres, 
                              Half = tempo)
}

gols_casa = do.call(rbind, gols_casa)
gols_fora = do.call(rbind, gols_fora)

goals = rbind(gols_casa, gols_fora) %>%
  arrange(Season, Match, Home_Team, Away_Team, Half, Minute, Stoppage_Time) %>%
  mutate(Season = as.integer(Season),
         Match = as.integer(Match),
         Score_Home = as.integer(Score_Home),
         Score_Away = as.integer(Score_Away),
         Stoppage_Time = as.integer(Stoppage_Time))

goals$Home_Team[which(goals$Home_Team == "America Fc - MG")] = "América - MG"
goals$Away_Team[which(goals$Away_Team == "America Fc - MG")] = "América - MG"
goals$Home_Team[which(goals$Home_Team == "Atletico - PR")] = "Athletico Paranaense - PR"
goals$Away_Team[which(goals$Away_Team == "Atletico - PR")] = "Athletico Paranaense - PR"
goals$Home_Team[which(goals$Home_Team == "Atlético - PR")] = "Athletico Paranaense - PR"
goals$Away_Team[which(goals$Away_Team == "Atlético - PR")] = "Athletico Paranaense - PR"

# Adicionando os que deram problema no scrape
r1 = tibble(Season = 2019, Match = 371,
            Date = "2019-12-08", Home_Team = "Internacional - RS", Score_Home = 2, Score_Away = 1,
            Away_Team = "Atlético - MG", Team = 2, Minute = 5, Stoppage_Time = NA, Half = 1)

r2 = tibble(Season = 2019, Match = 371,
            Date = "2019-12-08", Home_Team = "Internacional - RS", Score_Home = 2, Score_Away = 1,
            Away_Team = "Atlético - MG", Team = 1, Minute = 41, Stoppage_Time = NA, Half = 2)

r3 = tibble(Season = 2019, Match = 371,
            Date = "2019-12-08", Home_Team = "Internacional - RS", Score_Home = 2, Score_Away = 1,
            Away_Team = "Atlético - MG", Team = 1, Minute = 45, Stoppage_Time = 7, Half = 2)

r4 = tibble(Season = 2016, Match = 292,
            Date = "2016-10-13", Home_Team = "Fluminense - RJ", Score_Home = 1, Score_Away = 2, 
            Away_Team = "Flamengo - RJ", Team = 2, Minute = 12, Stoppage_Time = NA, Half = 1)

r5 = tibble(Season = 2016, Match = 292,
            Date = "2016-10-13", Home_Team = "Fluminense - RJ", Score_Home = 1, Score_Away = 2, 
            Away_Team = "Flamengo - RJ", Team = 1, Minute = 1, Stoppage_Time = NA, Half = 2)

r6 = tibble(Season = 2016, Match = 292,
            Date = "2016-10-13", Home_Team = "Fluminense - RJ", Score_Home = 1, Score_Away = 2,
            Away_Team = "Flamengo - RJ", Team = 2, Minute = 8, Stoppage_Time = NA, Half = 2)

goals = rbind(goals, r1, r2, r3, r4, r5, r6)
goals$Stoppage_Time[which(is.na(goals$Stoppage_Time))] = 0 # dava problema na ordem de jogos com goals aos 45 e aos 45+x
goals = goals %>%
  arrange(Date, Match, Half, Minute, Stoppage_Time)
goals$Stoppage_Time[which(goals$Stoppage_Time == 0)] = NA

save(goals, file = "scrape/data/goals.RData")
