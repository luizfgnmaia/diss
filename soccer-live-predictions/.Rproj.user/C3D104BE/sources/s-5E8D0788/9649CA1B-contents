
# Usar matches 93, 130 e 361

library(dplyr)
library(CVXR)
library(future.apply)
plan(multisession(workers = 12))

set.seed(1)

load("scrape/data/results.RData")
load("scrape/data/goals.RData")
load("scrape/data/reds.RData")
load("2020/data/input.RData")
load("pred 2020/data/mod_0.RData")
load("pred 2020/data/mod_3_2015_2019.RData")
load("pred 2020/data/mod_3_2015_2019_g.RData")
source("pred/pred_mod_0.R")
source("pred/pred_mod_3.R")
source("pred/input_pred.R")

results$Home_Team[which(results$Home_Team == "Athletico Paranaense - PR")] = "Athletico-PR"
results$Home_Team[which(results$Home_Team == "Atlético - GO")] = "Atlético-GO"
results$Home_Team[which(results$Home_Team == "Atlético - MG")] = "Atlético-MG"
results$Away_Team[which(results$Away_Team == "Athletico Paranaense - PR")] = "Athletico-PR"
results$Away_Team[which(results$Away_Team == "Atlético - GO")] = "Atlético-GO"
results$Away_Team[which(results$Away_Team == "Atlético - MG")] = "Atlético-MG"

results = results %>%
  mutate(Home_Team = stringr::str_replace_all(Home_Team, "\\s-\\s.*", ""),
         Away_Team = stringr::str_replace_all(Away_Team, "\\s-\\s.*", ""))

ind = copy_results %>%
  filter(Match %in% c(93, 130, 361)) %>%
  .$ind

# ind
# [1]  90 125 363

n_pred = 10^5

debug = list()

for(i in 1:length(ind)) {
  
  lst = list()
  
  lst$Match = results %>%
    filter(Season == 2020,
           Match == copy_results$Match[ind[i]])
  
  lst$pred_mod_0 = list()
  lst$pred_mod_3_2015_2019_g = list()
  
  for(m in 0:44) {
    input = input_pred(ind = ind[i], min = m)
    lst$pred_mod_0[[m+1]] = pred_mod_0(mod_0 = mod_0[[lst$Match$Date]], 
                                       n = n_pred, 
                                       home_team = lst$Match$Home_Team, 
                                       away_team = lst$Match$Away_Team,
                                       score_home = input$score_home,
                                       score_away = input$score_away,
                                       minute = m,
                                       half = 1)
    lst$pred_mod_3_2015_2019[[m+1]] = pred_mod_3(mod_3 = mod_3_2015_2019[[lst$Match$Date]], 
                                                 n = n_pred, 
                                                 home_team = lst$Match$Home_Team, 
                                                 away_team = lst$Match$Away_Team,
                                                 score_home = input$score_home,
                                                 score_away = input$score_away,
                                                 reds_home_1 = input$reds_home_1,
                                                 reds_away_1 = input$reds_away_1,
                                                 minute = m,
                                                 half = 1)
    lst$pred_mod_3_2015_2019_g[[m+1]] = pred_mod_3(mod_3 = mod_3_2015_2019_g[[lst$Match$Date]], 
                                                   n = n_pred, 
                                                   home_team = lst$Match$Home_Team, 
                                                   away_team = lst$Match$Away_Team,
                                                   score_home = input$score_home,
                                                   score_away = input$score_away,
                                                   reds_home_1 = input$reds_home_1,
                                                   reds_away_1 = input$reds_away_1,
                                                   minute = m,
                                                   half = 1)
    print(paste0("Partida: ", i, ", minuto: ", m))
  }
  
  for(m in 45:90) {
    input = input_pred(ind = ind[i], min = m)
    lst$pred_mod_0[[m+1]] = pred_mod_0(mod_0 = mod_0[[lst$Match$Date]], 
                                       n = n_pred, 
                                       home_team = lst$Match$Home_Team, 
                                       away_team = lst$Match$Away_Team,
                                       score_home = input$score_home,
                                       score_away = input$score_away,
                                       minute = m-45,
                                       half = 2)
    lst$pred_mod_3_2015_2019[[m+1]] = pred_mod_3(mod_3 = mod_3_2015_2019[[lst$Match$Date]], 
                                                 n = n_pred, 
                                                 home_team = lst$Match$Home_Team, 
                                                 away_team = lst$Match$Away_Team,
                                                 score_home = input$score_home,
                                                 score_away = input$score_away,
                                                 reds_home_1 = input$reds_home_1,
                                                 reds_away_1 = input$reds_away_1,
                                                 reds_home_2 = input$reds_home_2,
                                                 reds_away_2 = input$reds_away_2,
                                                 minute = m-45,
                                                 half = 2)
    lst$pred_mod_3_2015_2019_g[[m+1]] = pred_mod_3(mod_3 = mod_3_2015_2019_g[[lst$Match$Date]], 
                                                   n = n_pred, 
                                                   home_team = lst$Match$Home_Team, 
                                                   away_team = lst$Match$Away_Team,
                                                   score_home = input$score_home,
                                                   score_away = input$score_away,
                                                   reds_home_1 = input$reds_home_1,
                                                   reds_away_1 = input$reds_away_1,
                                                   reds_home_2 = input$reds_home_2,
                                                   reds_away_2 = input$reds_away_2,
                                                   minute = m-45,
                                                   half = 2)
    print(paste0("Partida: ", i, ", minuto: ", m))
  }
  debug[[i]] = lst
}

save(debug, file = "pred 2020/data/debug.RData")



