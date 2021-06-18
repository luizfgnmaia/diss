
library(dplyr)
library(future.apply)
plan(multisession(workers = 12)) 

load("transfermarkt/data/results.RData")
load("scrape/data/goals.RData")
load("scrape/data/reds.RData")
load("2015-2020/data/input.RData")
source("pred/pred_mod_12.R")
source("pred/input_pred.R")
source("pred/input_value.R")
load("weight/data/mod_12_dc.RData")
load("weight/data/matches_to_be_predicted.RData")

set.seed(1)

mod_12 = mod_12_dc
n_pred = 10^5

predictions_mod_12_dc = list()
lst = list()
season_match = NULL

for(i in 1:length(ind)) {
  lst$Match = results %>%
    filter(Season == copy_results$Season[ind[i]],
           Match == copy_results$Match[ind[i]])
  
  values = input_value(lst$Match$Home_Team, lst$Match$Away_Team, lst$Match$Date)
  
  season_match[i] = paste(lst$Match$Season, lst$Match$Match)
  
  lst$pred_0 = pred_mod_12(mod_12 = mod_12[[lst$Match$Date]], 
                           n = n_pred, 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           value_home = values$Value_Home,
                           value_away = values$Value_Away)
  
  input_15 = input_pred(ind = ind[i], min = 15)
  lst$pred_15 = pred_mod_12(mod_12 = mod_12[[lst$Match$Date]], 
                            n = n_pred, 
                            home_team = lst$Match$Home_Team, 
                            away_team = lst$Match$Away_Team,
                            score_home = input_15$score_home,
                            score_away = input_15$score_away,
                            reds_home_1 = input_15$reds_home_1,
                            reds_away_1 = input_15$reds_away_1,
                            minute = 15,
                            half = 1,
                            value_home = values$Value_Home,
                            value_away = values$Value_Away)
  
  input_30 = input_pred(ind = ind[i], min = 30)
  lst$pred_30 = pred_mod_12(mod_12 = mod_12[[lst$Match$Date]], 
                            n = n_pred, 
                            home_team = lst$Match$Home_Team, 
                            away_team = lst$Match$Away_Team,
                            score_home = input_30$score_home,
                            score_away = input_30$score_away,
                            reds_home_1 = input_30$reds_home_1,
                            reds_away_1 = input_30$reds_away_1,
                            minute = 30,
                            half = 1,
                            value_home = values$Value_Home,
                            value_away = values$Value_Away)
  
  input_45 = input_pred(ind = ind[i], min = 45)
  lst$pred_45 = pred_mod_12(mod_12 = mod_12[[lst$Match$Date]], 
                            n = n_pred, 
                            home_team = lst$Match$Home_Team, 
                            away_team = lst$Match$Away_Team,
                            score_home = input_45$score_home,
                            score_away = input_45$score_away,
                            reds_home_1 = input_45$reds_home_1,
                            reds_away_1 = input_45$reds_away_1,
                            minute = 0,
                            half = 2,
                            value_home = values$Value_Home,
                            value_away = values$Value_Away)
  
  input_60 = input_pred(ind = ind[i], min = 60)
  lst$pred_60 = pred_mod_12(mod_12 = mod_12[[lst$Match$Date]], 
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
                            half = 2,
                            value_home = values$Value_Home,
                            value_away = values$Value_Away)
  
  input_75 = input_pred(ind = ind[i], min = 75)
  lst$pred_75 = pred_mod_12(mod_12 = mod_12[[lst$Match$Date]], 
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
                            half = 2,
                            value_home = values$Value_Home,
                            value_away = values$Value_Away)
  
  predictions_mod_12_dc[[i]] = lst
  print(paste0(round(100*i/length(ind), 2), "%"))
}
names(predictions_mod_12_dc) = season_match

save(predictions_mod_12_dc, file = "weight/data/predictions_mod_12_dc.RData")

