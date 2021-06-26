
library(dplyr)
library(tidyr)
library(stringr)

files = paste0("odds/serie-a-open/serie-a_", 2016:2020, ".csv")

lst = list()

for(i in 1:length(files)) {
  lst[[i]] = read.csv2(files[i])
}

serie_a_open = do.call(rbind, lst) %>%
  mutate(Date = str_replace_all(Date, " ", "-"),
         Date = str_replace_all(Date, "Feb", "Fev"),
         Date = str_replace_all(Date, "Apr", "Abr"),
         Date = str_replace_all(Date, "May", "Mai"),
         Date = str_replace_all(Date, "Aug", "Ago"),
         Date = str_replace_all(Date, "Sep", "Set"),
         Date = str_replace_all(Date, "Oct", "Out"),
         Date = str_replace_all(Date, "Dec", "Dez"),
         Date = as.character(as.Date(Date, format = "%d-%b-%y")))

serie_a_open$Home_id[which(serie_a_open$Home_id == "Gremio")] = "Grêmio"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Sao Paulo")] = "São Paulo"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Sport Recife")] = "Sport"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Vitoria")] = "Vitória"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Atletico-MG")] = "Atlético-MG"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Flamengo RJ")] = "Flamengo"
serie_a_open$Home_id[which(serie_a_open$Home_id == "America MG")] = "América-MG"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Botafogo RJ")] = "Botafogo"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Chapecoense-SC")] = "Chapecoense"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Atletico GO")] = "Atlético-GO"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Avai")] = "Avaí"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Ceara")] = "Ceará"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Parana")] = "Paraná"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Goias")] = "Goiás"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Bragantino")] = "Red Bull Bragantino"
serie_a_open$Home_id[which(serie_a_open$Home_id == "CSA")] = "Csa"
serie_a_open$Home_id[which(serie_a_open$Home_id == "Vasco")] = "Vasco da Gama"

serie_a_open$Away_id[which(serie_a_open$Away_id == "Gremio")] = "Grêmio"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Sao Paulo")] = "São Paulo"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Sport Recife")] = "Sport"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Vitoria")] = "Vitória"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Atletico-MG")] = "Atlético-MG"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Flamengo RJ")] = "Flamengo"
serie_a_open$Away_id[which(serie_a_open$Away_id == "America MG")] = "América-MG"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Botafogo RJ")] = "Botafogo"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Chapecoense-SC")] = "Chapecoense"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Atletico GO")] = "Atlético-GO"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Avai")] = "Avaí"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Ceara")] = "Ceará"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Parana")] = "Paraná"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Goias")] = "Goiás"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Bragantino")] = "Red Bull Bragantino"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Csa")] = "Csa"
serie_a_open$Away_id[which(serie_a_open$Away_id == "Vasco")] = "Vasco da Gama"


serie_a_open = serie_a_open %>%
  rename(Home_Team = Home_id,
         Away_Team = Away_id,
         Score_Home = Score_home,
         Score_Away = Score_away) %>%
  mutate(OddHome = as.numeric(OddHome),
         OddDraw = as.numeric(OddDraw),
         OddAway = as.numeric(OddAway)) %>%
  rowwise() %>%
  mutate(Home = (1/OddHome) * 1/(1/OddHome + 1/OddDraw + 1/OddAway),
         Draw = (1/OddDraw) * 1/(1/OddHome + 1/OddDraw + 1/OddAway),
         Away = (1/OddAway) * 1/(1/OddHome + 1/OddDraw + 1/OddAway)) %>%
  select(-OddHome, -OddDraw, -OddAway)

save(serie_a_open, file = "odds/data/serie_a_open.RData")

table(serie_a_open$Bookmaker) %>%
  sort(decreasing = TRUE)

serie_a_open_means = serie_a_open %>%
  group_by(Season, Home_Team, Away_Team) %>%
  summarise(Home_open = mean(Home),
            Draw_open = mean(Draw),
            Away_open = mean(Away))

save(serie_a_open_means, file = "odds/data/serie_a_open_means.RData")