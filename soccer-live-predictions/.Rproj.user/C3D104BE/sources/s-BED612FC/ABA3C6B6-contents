
library(dplyr)

load("scrape/data/results.RData")

res = results %>%
  filter(Season == 2020) %>%
  arrange(Date)

dates = unique(res$Date)
n = 31

tmp1 = res %>%
  filter(Date <= dates[n]) %>%
  rename(Team = Home_Team) %>%
  count(Team, name = "n1")
tmp2 = res %>%
  filter(Date <= dates[n]) %>%
  rename(Team = Away_Team) %>%
  count(Team, name = "n2")
games_per_team = full_join(tmp1, tmp2) %>%
  rowwise() %>%
  mutate(n = sum(n1, n2, na.rm = TRUE)) %>%
  select(-n1, -n2) %>%
  arrange(n)
games_per_team

# dates[4] # a partir daqui todos os times tem 1 jogo
# dates[5] # começar daqui
# > dates[5]
# [1] "2020-08-15"
# ind = 17

# dates[31] # a partir daqui todos os times tem 10 jogos
# dates[32] # começar daqui
# > dates[32]
# [1] "2020-10-07"
# ind = 126

