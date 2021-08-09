
options(OutDec = ",")
library(dplyr)
library(xtable)

load("transfermarkt/data/results.RData")

qtd_mandante = results %>%
  count(Home_Team) %>%
  rename(Team = Home_Team,
         n_Home = n)

qtd_visitante = results %>%
  count(Away_Team) %>%
  rename(Team = Away_Team,
         n_Away = n)

soma_mandante = results %>%
  group_by(Home_Team) %>%
  summarise(sum_Home = sum(Value_Home)) %>%
  rename(Team = Home_Team)

soma_visitante = results %>%
  group_by(Away_Team) %>%
  summarise(sum_Away = sum(Value_Away)) %>%
  rename(Team = Away_Team)

tib = inner_join(qtd_mandante, qtd_visitante) %>%
  inner_join(soma_mandante) %>%
  inner_join(soma_visitante) %>%
  mutate(Média = (sum_Home + sum_Away) / (n_Home + n_Away),
         Temporadas = round(n_Home/19)) %>%
  arrange(desc(Média)) %>%
  select(Team, Média, Temporadas) %>%
  mutate(Temporadas = as.integer(Temporadas),
         Média = Média/10^6)

xtable(tib)




