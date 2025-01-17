
library(dplyr)

load("scrape/data/results.RData")

res = results %>%
  filter(Season == 2020) %>%
  arrange(Date)

dates = unique(res$Date)
n = 11

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

dates[11] # a partir daqui todos os times tem 4 jogos
dates[12] # começar daqui?

"2020-08-29"