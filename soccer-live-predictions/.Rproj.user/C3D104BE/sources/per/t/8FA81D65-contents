
library(dplyr)
library(readxl)

load("scrape/data/reds.RData")

xl = read_excel("scrape/reds_manual/reds.xlsx", sheet = "2016", 
                range = "A1:B380", col_names = FALSE)
xl = cbind(1:380, xl)
names(xl) = c("Match", "c1", "c2")
xl$c1[which(is.na(xl$c1))] = 0
xl$c2[which(is.na(xl$c2))] = 0

cnt = reds %>%
  filter(Season == 2016) %>%
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
  filter(Season == 2016, Match == 17) %>%
  View()

# Remover:
# Season = 2016, Match = 17, Date = "2016-05-22", 
# Home_Team = "São Paulo - SP", Score_Home = 1, Score_Away = 2, 
# Away_Team = "Internacional - RS", Minute = 40, Half = 2, Team = 2, Stoppage_Time = NA

reds %>%
  filter(Season == 2016, Match == 288) %>%
  View()

# Remover:
# Season = 2016, Match = 288, Date = "2016-10-09", 
# Home_Team = "América - MG", Score_Home = 0, Score_Away = 2, 
# Away_Team = "Palmeiras - SP", Minute = 9, Half = 2, Team = 1, Stoppage_Time = NA

