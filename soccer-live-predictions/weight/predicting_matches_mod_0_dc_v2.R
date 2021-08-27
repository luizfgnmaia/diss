
library(dplyr)

load("scrape/data/results2.RData")
load("scrape/data/goals.RData")
load("scrape/data/reds.RData")
load("2015-2020/data/input.RData")
source("pred/pred_mod_0_v2.R")
source("pred/input_pred.R")
load("weight/data/mod_0_dc.RData")
load("weight/data/matches_to_be_predicted.RData")

begin = Sys.time()

set.seed(1)

mod_0 = mod_0_dc

predictions_mod_0_dc = list()
lst = list()
season_match = NULL

for(i in 1:length(ind)) {
  lst$Match = results %>%
    filter(Season == copy_results$Season[ind[i]],
           Match == copy_results$Match[ind[i]])
  
  season_match[i] = paste(lst$Match$Season, lst$Match$Match)
  
  lst$pred_0 = pred_mod_0(mod_0 = mod_0[[lst$Match$Date]], 
                          home_team = lst$Match$Home_Team, 
                          away_team = lst$Match$Away_Team)
  
  input_15 = input_pred(ind = ind[i], min = 15)
  lst$pred_15 = pred_mod_0(mod_0 = mod_0[[lst$Match$Date]], 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           score_home = input_15$score_home,
                           score_away = input_15$score_away,
                           minute = 15,
                           half = 1)
  
  input_30 = input_pred(ind = ind[i], min = 30)
  lst$pred_30 = pred_mod_0(mod_0 = mod_0[[lst$Match$Date]], 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           score_home = input_30$score_home,
                           score_away = input_30$score_away,
                           minute = 30,
                           half = 1)
  
  input_45 = input_pred(ind = ind[i], min = 45)
  lst$pred_45 = pred_mod_0(mod_0 = mod_0[[lst$Match$Date]], 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           score_home = input_45$score_home,
                           score_away = input_45$score_away,
                           minute = 0,
                           half = 2)
  
  input_60 = input_pred(ind = ind[i], min = 60)
  lst$pred_60 = pred_mod_0(mod_0 = mod_0[[lst$Match$Date]], 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           score_home = input_60$score_home,
                           score_away = input_60$score_away,
                           minute = 15,
                           half = 2)
  
  input_75 = input_pred(ind = ind[i], min = 75)
  lst$pred_75 = pred_mod_0(mod_0 = mod_0[[lst$Match$Date]], 
                           home_team = lst$Match$Home_Team, 
                           away_team = lst$Match$Away_Team,
                           score_home = input_75$score_home,
                           score_away = input_75$score_away,
                           minute = 30,
                           half = 2)
  
  predictions_mod_0_dc[[i]] = lst
  print(paste0(round(100*i/length(ind), 2), "%"))
}
names(predictions_mod_0_dc) = season_match

duration = Sys.time() - begin
duration # Time difference of 10.62464 mins

save(predictions_mod_0_dc, file = "weight/data/predictions_mod_0_dc_v2.RData")





