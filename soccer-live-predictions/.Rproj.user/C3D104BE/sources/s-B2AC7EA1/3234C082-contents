
library(dplyr)
library(tidyr)
library(stringr)

files = paste0("odds/serie-a-open/serie-a_", 2016:2020, ".csv")

lst = list()

for(i in 1:length(files)) {
  lst[[i]] = read.csv2(files[i]) %>%
    filter(Bookmaker == "bet365") %>%
    pivot_wider(names_from = "Bookmaker", 
                values_from = c("OddHome", "OddDraw", "OddAway"))
}

bet365 = do.call(rbind, lst) %>%
  mutate(Date = str_replace_all(Date, " ", "-"),
         Date = str_replace_all(Date, "Feb", "Fev"),
         Date = str_replace_all(Date, "Apr", "Abr"),
         Date = str_replace_all(Date, "May", "Mai"),
         Date = str_replace_all(Date, "Aug", "Ago"),
         Date = str_replace_all(Date, "Sep", "Set"),
         Date = str_replace_all(Date, "Oct", "Out"),
         Date = str_replace_all(Date, "Dec", "Dez"),
         Date = as.character(as.Date(Date, format = "%d-%b-%y")))

bet365$Home_id[which(bet365$Home_id == "Gremio")] = "Grêmio"
bet365$Home_id[which(bet365$Home_id == "Sao Paulo")] = "São Paulo"
bet365$Home_id[which(bet365$Home_id == "Sport Recife")] = "Sport"
bet365$Home_id[which(bet365$Home_id == "Vitoria")] = "Vitória"
bet365$Home_id[which(bet365$Home_id == "Atletico-MG")] = "Atlético-MG"
bet365$Home_id[which(bet365$Home_id == "Flamengo RJ")] = "Flamengo"
bet365$Home_id[which(bet365$Home_id == "America MG")] = "América-MG"
bet365$Home_id[which(bet365$Home_id == "Botafogo RJ")] = "Botafogo"
bet365$Home_id[which(bet365$Home_id == "Chapecoense-SC")] = "Chapecoense"
bet365$Home_id[which(bet365$Home_id == "Atletico GO")] = "Atlético-GO"
bet365$Home_id[which(bet365$Home_id == "Avai")] = "Avaí"
bet365$Home_id[which(bet365$Home_id == "Ceara")] = "Ceará"
bet365$Home_id[which(bet365$Home_id == "Parana")] = "Paraná"
bet365$Home_id[which(bet365$Home_id == "Goias")] = "Goiás"
bet365$Home_id[which(bet365$Home_id == "Bragantino")] = "Red Bull Bragantino"
bet365$Home_id[which(bet365$Home_id == "CSA")] = "Csa"
bet365$Home_id[which(bet365$Home_id == "Vasco")] = "Vasco da Gama"

bet365$Away_id[which(bet365$Away_id == "Gremio")] = "Grêmio"
bet365$Away_id[which(bet365$Away_id == "Sao Paulo")] = "São Paulo"
bet365$Away_id[which(bet365$Away_id == "Sport Recife")] = "Sport"
bet365$Away_id[which(bet365$Away_id == "Vitoria")] = "Vitória"
bet365$Away_id[which(bet365$Away_id == "Atletico-MG")] = "Atlético-MG"
bet365$Away_id[which(bet365$Away_id == "Flamengo RJ")] = "Flamengo"
bet365$Away_id[which(bet365$Away_id == "America MG")] = "América-MG"
bet365$Away_id[which(bet365$Away_id == "Botafogo RJ")] = "Botafogo"
bet365$Away_id[which(bet365$Away_id == "Chapecoense-SC")] = "Chapecoense"
bet365$Away_id[which(bet365$Away_id == "Atletico GO")] = "Atlético-GO"
bet365$Away_id[which(bet365$Away_id == "Avai")] = "Avaí"
bet365$Away_id[which(bet365$Away_id == "Ceara")] = "Ceará"
bet365$Away_id[which(bet365$Away_id == "Parana")] = "Paraná"
bet365$Away_id[which(bet365$Away_id == "Goias")] = "Goiás"
bet365$Away_id[which(bet365$Away_id == "Bragantino")] = "Red Bull Bragantino"
bet365$Away_id[which(bet365$Away_id == "Csa")] = "Csa"
bet365$Away_id[which(bet365$Away_id == "Vasco")] = "Vasco da Gama"

bet365 = bet365 %>%
  rename(Home_Team = Home_id,
         Away_Team = Away_id,
         Score_Home = Score_home,
         Score_Away = Score_away) %>%
  mutate(OddHome_bet365 = as.numeric(OddHome_bet365),
         OddDraw_bet365 = as.numeric(OddDraw_bet365),
         OddAway_bet365 = as.numeric(OddAway_bet365)) %>%
  rowwise() %>%
  mutate(Home_bet365 = (1/OddHome_bet365) * 1/(1/OddHome_bet365 + 1/OddDraw_bet365 + 1/OddAway_bet365),
         Draw_bet365 = (1/OddDraw_bet365) * 1/(1/OddHome_bet365 + 1/OddDraw_bet365 + 1/OddAway_bet365),
         Away_bet365 = (1/OddAway_bet365) * 1/(1/OddHome_bet365 + 1/OddDraw_bet365 + 1/OddAway_bet365)) %>%
  select(-OddHome_bet365, -OddDraw_bet365, -OddAway_bet365)

save(bet365, file = "odds/data/bet365.RData")
