
library(dplyr)

load("scrape/data/results2.RData")
load("scrape/data/goals.RData")
load("scrape/data/reds.RData")
load("2015-2020/data/input.RData")
source("weight/fit_mod_0.R")
source("pred/pred_mod_0_v2.R")
load("weight/data/matches_to_be_predicted.RData")
load("weight/data/first_matches.RData")

pnk_Result <- function(Result, pH, pD, pA) {
  p = c(pH, pD, pA)
  p[Result]
}

pnk_Score <- function(Score, Match) {
  event = paste0(Match$Score_Home, "-", Match$Score_Away)
  p = Score[event]
  ifelse(is.na(p), 0, p)
}

csi_mod_0_pred_0 <- function(csi) {
  
  mod_0 = fit_mod_0_dates(csi)
  
  predictions_mod_0 = list()
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
    
    predictions_mod_0[[i]] = lst
    # print(paste0(round(100*i/length(ind), 2), "%"))
  }
  names(predictions_mod_0) = season_match
  
  HDA = tibble()
  for(i in 1:length(predictions_mod_0)) {
    tmp1 = predictions_mod_0[[i]]$Match %>%
      select(-Stoppage_Time_1, -Stoppage_Time_2) %>%
      mutate(Result = ifelse(Score_Home > Score_Away, 1,
                             ifelse(Score_Home == Score_Away, 2, 
                                    3)))
    
    Home_mod_0_pred_0 = predictions_mod_0[[i]]$pred_0$Result[[1]]
    Draw_mod_0_pred_0 = predictions_mod_0[[i]]$pred_0$Result[[2]]
    Away_mod_0_pred_0 = predictions_mod_0[[i]]$pred_0$Result[[3]]
    
    
    tmp2 = tibble(Home_mod_0_pred_0, Draw_mod_0_pred_0, Away_mod_0_pred_0)
    
    hda = cbind(tmp1, tmp2)
    HDA = rbind(HDA, hda)
  }
  
  pnk_Score_mod_0_pred_0 = NULL
  
  for(i in 1:nrow(HDA)) {
    pnk_Score_mod_0_pred_0[i] = pnk_Score(predictions_mod_0[[i]]$pred_0$Score, predictions_mod_0[[i]]$Match)
  }
  
  HDA = HDA %>%
    rowwise() %>%
    mutate(pnk_Result_mod_0_pred_0 = pnk_Result(Result, Home_mod_0_pred_0, Draw_mod_0_pred_0, Away_mod_0_pred_0)) %>%
    cbind(pnk_Score_mod_0_pred_0) %>%
    anti_join(first_matches)
  
  HDA$pnk_Result_mod_0_pred_0[which(HDA$pnk_Result_mod_0_pred_0 == 0)] = 1/10^5
  HDA$pnk_Score_mod_0_pred_0[which(HDA$pnk_Score_mod_0_pred_0 == 0)] = 1/10^5
  
  GeoMean_Results = EnvStats::geoMean(HDA$pnk_Result_mod_0_pred_0)
  GeoMean_Scores = EnvStats::geoMean(HDA$pnk_Score_mod_0_pred_0)
  
  ret = list(HDA = HDA,
             GeoMean_Results = GeoMean_Results,
             GeoMean_Scores = GeoMean_Scores)
  
  ret
}


# 0.0065/3.5 = 0.001857143
values_to_test = seq(from = 0, to = 0.004, by = 0.0001)

begin = Sys.time()
lst_csi_mod_0_pred_0 = list()
for(i in 1:length(values_to_test)) {
  lst_csi_mod_0_pred_0[[i]] = csi_mod_0_pred_0(values_to_test[i])
  print(paste0(round(100*i/length(values_to_test), 2), "%"))
  print(lst_csi_mod_0_pred_0[[i]][2])
  print(lst_csi_mod_0_pred_0[[i]][3])
  print(Sys.time() - begin)
}

names(lst_csi_mod_0_pred_0) = values_to_test

save(lst_csi_mod_0_pred_0, file = "weight/data/lst_csi_mod_0_pred_0_v2.RData")
