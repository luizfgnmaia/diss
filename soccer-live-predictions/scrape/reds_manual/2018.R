
library(dplyr)
library(readxl)

load("scrape/data/reds.RData")

xl = read_excel("scrape/reds_manual/reds.xlsx", sheet = "2018", 
                range = "A1:B380", col_names = FALSE)
xl = cbind(1:380, xl)
names(xl) = c("Match", "c1", "c2")
xl$c1[which(is.na(xl$c1))] = 0
xl$c2[which(is.na(xl$c2))] = 0

cnt = reds %>%
  filter(Season == 2018) %>%
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
  filter(Season == 2018, Match == 79) %>%
  View()

# Remover:
# Season = 2018, Match = 79, Date = "2018-05-31", 
# Home_Team = "Corinthians - SP", Score_Home = 1, Score_Away = 0, 
# Away_Team = "América - MG", Minute = 43, Half = 2, Team = 2, Stoppage_Time = NA

reds %>%
  filter(Season == 2018, Match == 119) %>%
  View()

# Remover: (cuidado que existem várias expulsões no mesmo minuto nesse jogo, o anti join não vai funcionar)
# Season = 2018, Match = 119, Date = "2018-06-13", 
# Home_Team = "Palmeiras - SP", Score_Home = 1, Score_Away = 1, 
# Away_Team = "Flamengo - RJ", Minute = 45, Half = 2, Team = 2, Stoppage_Time = 4

# Remover: (cuidado que existem várias expulsões no mesmo minuto nesse jogo, o anti join não vai funcionar)
# Season = 2018, Match = 119, Date = "2018-06-13", 
# Home_Team = "Palmeiras - SP", Score_Home = 1, Score_Away = 1, 
# Away_Team = "Flamengo - RJ", Minute = 45, Half = 2, Team = 1, Stoppage_Time = 4

reds %>%
  filter(Season == 2018, Match == 141) %>%
  View()

# Adicionar:
# Season = 2018, Match = 141, Date = "2018-07-25", 
# Home_Team = "Atlético - MG", Score_Home = 2, Score_Away = 0, 
# Away_Team = "Paraná - PR", Minute = 35, Half = 2, Team = 1, Stoppage_Time = NA

reds %>%
  filter(Season == 2018, Match == 173) %>%
  View()

# Remover:
# Season = 2018, Match = 173, Date = "2018-08-12", 
# Home_Team = "Chapecoense - SC", Score_Home = 2, Score_Away = 1, 
# Away_Team = "Corinthians - SP", Minute = 39, Half = 1, Team = 1, Stoppage_Time = NA

reds %>%
  filter(Season == 2018, Match == 243) %>%
  View()

# Remover:
# Season = 2018, Match = 243, Date = "2018-09-16", 
# Home_Team = "Botafogo - RJ", Score_Home = 1, Score_Away = 0, 
# Away_Team = "América - MG", Minute = 42, Half = 2, Team = 1, Stoppage_Time = NA

reds %>%
  filter(Season == 2018, Match == 314) %>%
  View()

# Remover:
# Season = 2018, Match = 314, Date = "2018-11-04", 
# Home_Team = "Paraná - PR", Score_Home = 1, Score_Away = 1, 
# Away_Team = "Vitória - BA", Minute = 22, Half = 2, Team = 2, Stoppage_Time = NA

