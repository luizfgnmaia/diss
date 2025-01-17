
library(dplyr)

load("scrape/data/scrape.RData")
load("scrape/data/scrape_st.RData")
source("scrape/fix_date.R")

partida = rep(1:380, 6)
ano = c(rep(2015, 380), rep(2016, 380), rep(2017, 380), rep(2018, 380), rep(2019, 380), rep(2020, 380))

data = fix_date(data)

results = tibble(Season = ano,
                 Match = partida,
                 Date = data,
                 Home_Team = time_1,
                 Score_Home = placar_1,
                 Score_Away = placar_2,
                 Away_Team = time_2,
                 Stoppage_Time_1 = first_half,
                 Stoppage_Time_2 = second_half) %>%
  mutate(Season = as.integer(Season),
         Score_Home = as.integer(Score_Home),
         Score_Away = as.integer(Score_Away)) %>%
  filter(!is.na(Score_Home)) # tirando a partida da Chape

results$Home_Team[which(results$Home_Team == "America Fc - MG")] = "América - MG"
results$Away_Team[which(results$Away_Team == "America Fc - MG")] = "América - MG"
results$Home_Team[which(results$Home_Team == "Atletico - PR")] = "Athletico Paranaense - PR"
results$Away_Team[which(results$Away_Team == "Atletico - PR")] = "Athletico Paranaense - PR"
results$Home_Team[which(results$Home_Team == "Atlético - PR")] = "Athletico Paranaense - PR"
results$Away_Team[which(results$Away_Team == "Atlético - PR")] = "Athletico Paranaense - PR"

save(results, file = "scrape/data/results.RData")
