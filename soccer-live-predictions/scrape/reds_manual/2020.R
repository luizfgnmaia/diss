
library(dplyr)
library(readxl)

load("scrape/data/reds.RData")

xl = read_excel("scrape/reds_manual/reds.xlsx", sheet = "2020", 
                range = "A1:B380", col_names = FALSE)
xl = cbind(1:380, xl)
names(xl) = c("Match", "c1", "c2")
xl$c1[which(is.na(xl$c1))] = 0
xl$c2[which(is.na(xl$c2))] = 0

cnt = reds %>%
  filter(Season == 2020) %>%
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
  filter(Season == 2020, Match == 160) %>%
  View()

# Adicionar:
# Season = 2020, Match = 160, Date = "2020-10-16", 
# Home_Team = "Goiás - GO", Score_Home = 1, Score_Away = 1, 
# Away_Team = "Bahia - BA", Minute = 29, Half = 2, Team = 2, Stoppage_Time = NA

reds %>%
  filter(Season == 2020, Match == 170) %>%
  View()

# Remover:
# Season = 2020, Match = 170, Date = "2020-10-17", 
# Home_Team = "Atlético - GO", Score_Home = 1, Score_Away = 1, 
# Away_Team = "Athletico Paranaense - PR", Minute = 45, Half = 2, Team = 1, Stoppage_Time = 4

reds %>%
  filter(Season == 2020, Match == 319) %>%
  View()

# Remover:
# Season = 2020, Match = 319, Date = "2021-01-24", 
# Home_Team = "Ceará - CE", Score_Home = 2, Score_Away = 1, 
# Away_Team = "Palmeiras - SP", Minute = 37, Half = 2, Team = 1, Stoppage_Time = NA

