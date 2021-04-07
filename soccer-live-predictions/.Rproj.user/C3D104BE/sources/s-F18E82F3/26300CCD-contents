
library(dplyr)

load("2020/data/predictions_mod_0.RData")
load("2020/data/predictions_mod_1.RData")

n_pred = 100000

HDA = tibble()

for(i in 1:length(predictions_mod_0)) {
  
  tmp1 = predictions_mod_0[[i]]$Match %>%
    select(-Stoppage_Time_1, -Stoppage_Time_2) %>%
    mutate(Result = ifelse(Score_Home > Score_Away, 1,
                           ifelse(Score_Home == Score_Away, 2, 
                                  3)))
  
  home_mod_0_0 = sum(predictions_mod_0[[i]]$pred_0[,1] > predictions_mod_0[[i]]$pred_0[,2]) / n_pred
  away_mod_0_0 = sum(predictions_mod_0[[i]]$pred_0[,1] < predictions_mod_0[[i]]$pred_0[,2]) / n_pred
  draw_mod_0_0 = 1 - home_mod_0_0 - away_mod_0_0
  
  home_mod_0_15 = sum(predictions_mod_0[[i]]$pred_15[,1] > predictions_mod_0[[i]]$pred_15[,2]) / n_pred
  away_mod_0_15 = sum(predictions_mod_0[[i]]$pred_15[,1] < predictions_mod_0[[i]]$pred_15[,2]) / n_pred
  draw_mod_0_15 = 1 - home_mod_0_15 - away_mod_0_15
  
  home_mod_0_30 = sum(predictions_mod_0[[i]]$pred_30[,1] > predictions_mod_0[[i]]$pred_30[,2]) / n_pred
  away_mod_0_30 = sum(predictions_mod_0[[i]]$pred_30[,1] < predictions_mod_0[[i]]$pred_30[,2]) / n_pred
  draw_mod_0_30 = 1 - home_mod_0_30 - away_mod_0_30
  
  home_mod_0_45 = sum(predictions_mod_0[[i]]$pred_45[,1] > predictions_mod_0[[i]]$pred_45[,2]) / n_pred
  away_mod_0_45 = sum(predictions_mod_0[[i]]$pred_45[,1] < predictions_mod_0[[i]]$pred_45[,2]) / n_pred
  draw_mod_0_45 = 1 - home_mod_0_45 - away_mod_0_45
  
  home_mod_0_60 = sum(predictions_mod_0[[i]]$pred_60[,1] > predictions_mod_0[[i]]$pred_60[,2]) / n_pred
  away_mod_0_60 = sum(predictions_mod_0[[i]]$pred_60[,1] < predictions_mod_0[[i]]$pred_60[,2]) / n_pred
  draw_mod_0_60 = 1 - home_mod_0_60 - away_mod_0_60
  
  home_mod_0_75 = sum(predictions_mod_0[[i]]$pred_75[,1] > predictions_mod_0[[i]]$pred_75[,2]) / n_pred
  away_mod_0_75 = sum(predictions_mod_0[[i]]$pred_75[,1] < predictions_mod_0[[i]]$pred_75[,2]) / n_pred
  draw_mod_0_75 = 1 - home_mod_0_75 - away_mod_0_75
  
  home_mod_1_0 = sum(predictions_mod_1[[i]]$pred_0[,1] > predictions_mod_1[[i]]$pred_0[,2]) / n_pred
  away_mod_1_0 = sum(predictions_mod_1[[i]]$pred_0[,1] < predictions_mod_1[[i]]$pred_0[,2]) / n_pred
  draw_mod_1_0 = 1 - home_mod_1_0 - away_mod_1_0
  
  home_mod_1_15 = sum(predictions_mod_1[[i]]$pred_15[,1] > predictions_mod_1[[i]]$pred_15[,2]) / n_pred
  away_mod_1_15 = sum(predictions_mod_1[[i]]$pred_15[,1] < predictions_mod_1[[i]]$pred_15[,2]) / n_pred
  draw_mod_1_15 = 1 - home_mod_1_15 - away_mod_1_15
  
  home_mod_1_30 = sum(predictions_mod_1[[i]]$pred_30[,1] > predictions_mod_1[[i]]$pred_30[,2]) / n_pred
  away_mod_1_30 = sum(predictions_mod_1[[i]]$pred_30[,1] < predictions_mod_1[[i]]$pred_30[,2]) / n_pred
  draw_mod_1_30 = 1 - home_mod_1_30 - away_mod_1_30
  
  home_mod_1_45 = sum(predictions_mod_1[[i]]$pred_45[,1] > predictions_mod_1[[i]]$pred_45[,2]) / n_pred
  away_mod_1_45 = sum(predictions_mod_1[[i]]$pred_45[,1] < predictions_mod_1[[i]]$pred_45[,2]) / n_pred
  draw_mod_1_45 = 1 - home_mod_1_45 - away_mod_1_45
  
  home_mod_1_60 = sum(predictions_mod_1[[i]]$pred_60[,1] > predictions_mod_1[[i]]$pred_60[,2]) / n_pred
  away_mod_1_60 = sum(predictions_mod_1[[i]]$pred_60[,1] < predictions_mod_1[[i]]$pred_60[,2]) / n_pred
  draw_mod_1_60 = 1 - home_mod_1_60 - away_mod_1_60
  
  home_mod_1_75 = sum(predictions_mod_1[[i]]$pred_75[,1] > predictions_mod_1[[i]]$pred_75[,2]) / n_pred
  away_mod_1_75 = sum(predictions_mod_1[[i]]$pred_75[,1] < predictions_mod_1[[i]]$pred_75[,2]) / n_pred
  draw_mod_1_75 = 1 - home_mod_1_75 - away_mod_1_75
  
  tmp2 = tibble(Home_mod0_pred0 = home_mod_0_0,
                Draw_mod0_pred0 = draw_mod_0_0,
                Away_mod0_pred0 = away_mod_0_0,
                Home_mod0_pred15 = home_mod_0_15,
                Draw_mod0_pred15 = draw_mod_0_15,
                Away_mod0_pred15 = away_mod_0_15,
                Home_mod0_pred30 = home_mod_0_30,
                Draw_mod0_pred30 = draw_mod_0_30,
                Away_mod0_pred30 = away_mod_0_30,
                Home_mod0_pred45 = home_mod_0_45,
                Draw_mod0_pred45 = draw_mod_0_45,
                Away_mod0_pred45 = away_mod_0_45,
                Home_mod0_pred60 = home_mod_0_60,
                Draw_mod0_pred60 = draw_mod_0_60,
                Away_mod0_pred60 = away_mod_0_60,
                Home_mod0_pred75 = home_mod_0_75,
                Draw_mod0_pred75 = draw_mod_0_75,
                Away_mod0_pred75 = away_mod_0_75,
                Home_mod1_pred0 = home_mod_1_0,
                Draw_mod1_pred0 = draw_mod_1_0,
                Away_mod1_pred0 = away_mod_1_0,
                Home_mod1_pred15 = home_mod_1_15,
                Draw_mod1_pred15 = draw_mod_1_15,
                Away_mod1_pred15 = away_mod_1_15,
                Home_mod1_pred30 = home_mod_1_30,
                Draw_mod1_pred30 = draw_mod_1_30,
                Away_mod1_pred30 = away_mod_1_30,
                Home_mod1_pred45 = home_mod_1_45,
                Draw_mod1_pred45 = draw_mod_1_45,
                Away_mod1_pred45 = away_mod_1_45,
                Home_mod1_pred60 = home_mod_1_60,
                Draw_mod1_pred60 = draw_mod_1_60,
                Away_mod1_pred60 = away_mod_1_60,
                Home_mod1_pred75 = home_mod_1_75,
                Draw_mod1_pred75 = draw_mod_1_75,
                Away_mod1_pred75 = away_mod_1_75)
  
  hda = cbind(tmp1, tmp2)
  HDA = rbind(HDA, hda)
}

RPS <- function(Result, pH, pD, pA) {
  verification::rps(obs = Result, pred = matrix(c(pH, pD, pA), nrow = 1))$rps
}

HDA = HDA %>%
  rowwise() %>%
  mutate(RPS_mod0_pred0 = RPS(Result, Home_mod0_pred0, Draw_mod0_pred0, Away_mod0_pred0),
         RPS_mod0_pred15 = RPS(Result, Home_mod0_pred15, Draw_mod0_pred15, Away_mod0_pred15),
         RPS_mod0_pred30 = RPS(Result, Home_mod0_pred30, Draw_mod0_pred30, Away_mod0_pred30),
         RPS_mod0_pred45 = RPS(Result, Home_mod0_pred45, Draw_mod0_pred45, Away_mod0_pred45),
         RPS_mod0_pred60 = RPS(Result, Home_mod0_pred60, Draw_mod0_pred60, Away_mod0_pred60),
         RPS_mod0_pred75 = RPS(Result, Home_mod0_pred75, Draw_mod0_pred75, Away_mod0_pred75),
         RPS_mod1_pred0 = RPS(Result, Home_mod1_pred0, Draw_mod1_pred0, Away_mod1_pred0),
         RPS_mod1_pred15 = RPS(Result, Home_mod1_pred15, Draw_mod1_pred15, Away_mod1_pred15),
         RPS_mod1_pred30 = RPS(Result, Home_mod1_pred30, Draw_mod1_pred30, Away_mod1_pred30),
         RPS_mod1_pred45 = RPS(Result, Home_mod1_pred45, Draw_mod1_pred45, Away_mod1_pred45),
         RPS_mod1_pred60 = RPS(Result, Home_mod1_pred60, Draw_mod1_pred60, Away_mod1_pred60),
         RPS_mod1_pred75 = RPS(Result, Home_mod1_pred75, Draw_mod1_pred75, Away_mod1_pred75))

save(HDA, file = "2020/data/HDA.RData")


