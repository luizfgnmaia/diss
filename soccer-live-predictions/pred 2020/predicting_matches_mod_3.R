
library(dplyr)
library(CVXR)
library(future.apply)
plan(multisession(workers = 12))

set.seed(1)

load("scrape/data/results.RData")
load("scrape/data/goals.RData")
load("scrape/data/reds.RData")
load("2020/data/input.RData")
load("pred 2020/data/mod_3.RData")
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

ind = 48:380
n_pred = 10^5
match = NULL

predictions_mod_3 = list()

for(i in 1:length(ind)) {
  
  lst = list()
  
  lst$Match = results %>%
    filter(Season == 2020,
           Match == copy_results$Match[ind[i]])
  
  match[i] = lst$Match$Match
  
  lst$pred_0 = pred_mod_3(mod_3 = mod_3[[lst$Match$Date]], 
                          n = n_pred, 
                          home_team = lst$Match$Home_Team, 
                          away_team = lst$Match$Away_Team)
  
  input_15 = input_pred(ind = ind[i], min = 15)
  lst$pred_15 = pred_mod_3(mod_3 = mod_3[[lst$Match$Date]], 
                           n = n_pred, 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           score_home = input_15$score_home,
                           score_away = input_15$score_away,
                           reds_home_1 = input_15$reds_home_1,
                           reds_away_1 = input_15$reds_away_1,
                           minute = 15,
                           half = 1)
  
  input_30 = input_pred(ind = ind[i], min = 30)
  lst$pred_30 = pred_mod_3(mod_3 = mod_3[[lst$Match$Date]], 
                           n = n_pred, 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           score_home = input_30$score_home,
                           score_away = input_30$score_away,
                           reds_home_1 = input_30$reds_home_1,
                           reds_away_1 = input_30$reds_away_1,
                           minute = 30,
                           half = 1)
  
  input_45 = input_pred(ind = ind[i], min = 45)
  lst$pred_45 = pred_mod_3(mod_3 = mod_3[[lst$Match$Date]], 
                           n = n_pred, 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           score_home = input_45$score_home,
                           score_away = input_45$score_away,
                           reds_home_1 = input_45$reds_home_1,
                           reds_away_1 = input_45$reds_away_1,
                           minute = 0,
                           half = 2)
  
  input_60 = input_pred(ind = ind[i], min = 60)
  lst$pred_60 = pred_mod_3(mod_3 = mod_3[[lst$Match$Date]], 
                           n = n_pred, 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           score_home = input_60$score_home,
                           score_away = input_60$score_away,
                           reds_home_1 = input_60$reds_home_1,
                           reds_away_1 = input_60$reds_away_1,
                           reds_home_2 = input_60$reds_home_2,
                           reds_away_2 = input_60$reds_away_2,
                           minute = 15,
                           half = 2)
  
  input_75 = input_pred(ind = ind[i], min = 75)
  lst$pred_75 = pred_mod_3(mod_3 = mod_3[[lst$Match$Date]], 
                           n = n_pred, 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           score_home = input_75$score_home,
                           score_away = input_75$score_away,
                           reds_home_1 = input_75$reds_home_1,
                           reds_away_1 = input_75$reds_away_1,
                           reds_home_2 = input_75$reds_home_2,
                           reds_away_2 = input_75$reds_away_2,
                           minute = 30,
                           half = 2)
  
  predictions_mod_3[[i]] = lst
  print(paste0(round(100*i/length(ind), 2), "%"))
}

names(predictions_mod_3) = match

save(predictions_mod_3, file = "pred 2020/data/predictions_mod_3.RData")
