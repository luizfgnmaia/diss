
library(dplyr)
library(readxl)

load("scrape/data/reds.RData")

xl = read_excel("scrape/reds_manual/reds.xlsx", sheet = "2015", 
                range = "A1:B380", col_names = FALSE)
xl = cbind(1:380, xl)
names(xl) = c("Match", "c1", "c2")
xl$c1[which(is.na(xl$c1))] = 0
xl$c2[which(is.na(xl$c2))] = 0

cnt = reds %>%
  filter(Season == 2015) %>%
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
  filter(Season == 2015, Match == 40) %>%
  View()

# Remover:
# Season = 2015, Match = 40, Date = "2015-05-31", 
# Home_Team = "Figueirense - SC", Score_Home = 2, Score_Away = 1, 
# Away_Team = "Cruzeiro - MG", Minute = 25, Half = 2, Team = 2, Stoppage_Time = NA

reds %>%
  filter(Season == 2015, Match == 265) %>%
  View()

# Remover:
# Season = 2015, Match = 265, Date = "2015-09-20", 
# Home_Team = "Corinthians - SP", Score_Home = 2, Score_Away = 0, 
# Away_Team = "Santos - SP", Minute = 45, Half = 1, Team = 2, Stoppage_Time = 2

reds %>%
  filter(Season == 2015, Match == 284) %>%
  View()

# Remover:
# Season = 2015, Match = 284, Date = "2015-10-04", 
# Home_Team = "Avaí - SC", Score_Home = 1, Score_Away = 1, 
# Away_Team = "Vasco da Gama - RJ", Minute = 33, Half = 2, Team = 2, Stoppage_Time = NA

reds %>%
  filter(Season == 2015, Match == 340) %>%
  View()

# Remover:
# Season = 2015, Match = 340, Date = "2015-11-08", 
# Home_Team = "Joinville - SC", Score_Home = 0, Score_Away = 0, 
# Away_Team = "Santos - SP", Minute = 9, Half = 1, Team = 1, Stoppage_Time = NA

reds %>%
  filter(Season == 2015, Match == 360) %>%
  View()

# Remover:
# Season = 2015, Match = 360, Date = "2015-11-22", 
# Home_Team = "Coritiba - PR", Score_Home = 1, Score_Away = 0, 
# Away_Team = "Santos - SP", Minute = 24, Half = 2, Team = 2, Stoppage_Time = NA

