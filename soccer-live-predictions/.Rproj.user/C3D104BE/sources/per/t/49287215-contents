
library(dplyr)
library(readxl)

load("scrape/data/reds.RData")

xl = read_excel("scrape/reds_manual/reds.xlsx", sheet = "2017", 
                range = "A1:B380", col_names = FALSE)
xl = cbind(1:380, xl)
names(xl) = c("Match", "c1", "c2")
xl$c1[which(is.na(xl$c1))] = 0
xl$c2[which(is.na(xl$c2))] = 0

cnt = reds %>%
  filter(Season == 2017) %>%
  count(Match)

fj = full_join(cnt, xl)
fj$n[is.na(fj$n)] = 0
fj = fj %>%
  arrange(Match)

fjV = fj %>%
  rowwise() %>%
  mutate(c = c1-c2,
         dif = n - c) %>%
  filter(dif != 0)

View(fjV)

reds %>%
  filter(Season == 2017, Match == 60) %>%
  View()

# Remover:
# Season = 2017, Match = 60, Date = "2017-06-11", 
# Home_Team = "Avaí - SC", Score_Home = 1, Score_Away = 1, 
# Away_Team = "Flamengo - RJ", Minute = 40, Half = 2, Team = 1, Stoppage_Time = NA

reds %>%
  filter(Season == 2017, Match == 267) %>%
  View()

# Remover:
# Season = 2017, Match = 267, Date = "2017-10-11", 
# Home_Team = "Avaí - SC", Score_Home = 1, Score_Away = 2, 
# Away_Team = "Vasco da Gama - RJ", Minute = 1, Half = 2, Team = 1, Stoppage_Time = NA

reds %>%
  filter(Season == 2017, Match == 378) %>%
  View()

# Adicionar:
# Season = 2017, Match = 378, Date = "2017-12-03", 
# Home_Team = "Vitória - BA", Score_Home = 1, Score_Away = 2, 
# Away_Team = "Flamengo - RJ", Minute = 45, Half = 2, Team = 1, Stoppage_Time = 4

