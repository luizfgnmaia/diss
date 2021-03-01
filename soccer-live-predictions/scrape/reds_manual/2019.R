
library(dplyr)
library(readxl)

load("scrape/data/reds.RData")

xl = read_excel("scrape/reds_manual/reds.xlsx", sheet = "2019", 
                range = "A1:B380", col_names = FALSE)
xl = cbind(1:380, xl)
names(xl) = c("Match", "c1", "c2")
xl$c1[which(is.na(xl$c1))] = 0
xl$c2[which(is.na(xl$c2))] = 0

cnt = reds %>%
  filter(Season == 2019) %>%
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
  filter(Season == 2019, Match == 22) %>%
  View()

# Remover
# Season = 2019, Match = 22, Date = "2019-05-05", 
# Home_Team = "Cruzeiro - MG", Score_Home = 2, Score_Away = 1, 
# Away_Team = "Goiás - GO", Minute = 45, Half = 2, Team = 2, Stoppage_Time = 2

reds %>%
  filter(Season == 2019, Match == 254) %>%
  View()

# Remover
# Season = 2019, Match = 254, Date = "2019-10-16", 
# Home_Team = "Palmeiras - SP", Score_Home = 1, Score_Away = 0, 
# Away_Team = "Chapecoense - SC", Minute = 26, Half = 2, Team = 1, Stoppage_Time = NA
