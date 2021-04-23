
library(dplyr)

load("pred 2019/data/predictions_mod_0.RData")
load("pred 2019/data/predictions_mod_3_2015_2018.RData")

HDA = tibble()

for(i in 1:length(predictions_mod_0)) {
  
  tmp1 = predictions_mod_0[[i]]$Match %>%
    select(-Stoppage_Time_1, -Stoppage_Time_2) %>%
    mutate(Result = ifelse(Score_Home > Score_Away, 1,
                           ifelse(Score_Home == Score_Away, 2, 
                                  3)))
  
  home_m0_0 = predictions_mod_0[[i]]$pred_0$Result[[1]]
  draw_m0_0 = predictions_mod_0[[i]]$pred_0$Result[[2]]
  away_m0_0 = predictions_mod_0[[i]]$pred_0$Result[[3]]
  
  home_m0_15 = predictions_mod_0[[i]]$pred_15$Result[[1]]
  draw_m0_15 = predictions_mod_0[[i]]$pred_15$Result[[2]]
  away_m0_15 = predictions_mod_0[[i]]$pred_15$Result[[3]]
  
  home_m0_30 = predictions_mod_0[[i]]$pred_30$Result[[1]]
  draw_m0_30 = predictions_mod_0[[i]]$pred_30$Result[[2]]
  away_m0_30 = predictions_mod_0[[i]]$pred_30$Result[[3]]
  
  home_m0_45 = predictions_mod_0[[i]]$pred_45$Result[[1]]
  draw_m0_45 = predictions_mod_0[[i]]$pred_45$Result[[2]]
  away_m0_45 = predictions_mod_0[[i]]$pred_45$Result[[3]]
  
  home_m0_60 = predictions_mod_0[[i]]$pred_60$Result[[1]]
  draw_m0_60 = predictions_mod_0[[i]]$pred_60$Result[[2]]
  away_m0_60 = predictions_mod_0[[i]]$pred_60$Result[[3]]
  
  home_m0_75 = predictions_mod_0[[i]]$pred_75$Result[[1]]
  draw_m0_75 = predictions_mod_0[[i]]$pred_75$Result[[2]]
  away_m0_75 = predictions_mod_0[[i]]$pred_75$Result[[3]]
  
  home_m31518_0 = predictions_mod_3_2015_2018[[i]]$pred_0$Result[[1]]
  draw_m31518_0 = predictions_mod_3_2015_2018[[i]]$pred_0$Result[[2]]
  away_m31518_0 = predictions_mod_3_2015_2018[[i]]$pred_0$Result[[3]]
  
  home_m31518_15 = predictions_mod_3_2015_2018[[i]]$pred_15$Result[[1]]
  draw_m31518_15 = predictions_mod_3_2015_2018[[i]]$pred_15$Result[[2]]
  away_m31518_15 = predictions_mod_3_2015_2018[[i]]$pred_15$Result[[3]]
  
  home_m31518_30 = predictions_mod_3_2015_2018[[i]]$pred_30$Result[[1]]
  draw_m31518_30 = predictions_mod_3_2015_2018[[i]]$pred_30$Result[[2]]
  away_m31518_30 = predictions_mod_3_2015_2018[[i]]$pred_30$Result[[3]]
  
  home_m31518_45 = predictions_mod_3_2015_2018[[i]]$pred_45$Result[[1]]
  draw_m31518_45 = predictions_mod_3_2015_2018[[i]]$pred_45$Result[[2]]
  away_m31518_45 = predictions_mod_3_2015_2018[[i]]$pred_45$Result[[3]]
  
  home_m31518_60 = predictions_mod_3_2015_2018[[i]]$pred_60$Result[[1]]
  draw_m31518_60 = predictions_mod_3_2015_2018[[i]]$pred_60$Result[[2]]
  away_m31518_60 = predictions_mod_3_2015_2018[[i]]$pred_60$Result[[3]]
  
  home_m31518_75 = predictions_mod_3_2015_2018[[i]]$pred_75$Result[[1]]
  draw_m31518_75 = predictions_mod_3_2015_2018[[i]]$pred_75$Result[[2]]
  away_m31518_75 = predictions_mod_3_2015_2018[[i]]$pred_75$Result[[3]]
  
  tmp2 = tibble(Home_mod_0_pred_0 = home_m0_0,
                Draw_mod_0_pred_0 = draw_m0_0,
                Away_mod_0_pred_0 = away_m0_0,
                Home_mod_0_pred_15 = home_m0_15,
                Draw_mod_0_pred_15 = draw_m0_15,
                Away_mod_0_pred_15 = away_m0_15,
                Home_mod_0_pred_30 = home_m0_30,
                Draw_mod_0_pred_30 = draw_m0_30,
                Away_mod_0_pred_30 = away_m0_30,
                Home_mod_0_pred_45 = home_m0_45,
                Draw_mod_0_pred_45 = draw_m0_45,
                Away_mod_0_pred_45 = away_m0_45,
                Home_mod_0_pred_60 = home_m0_60,
                Draw_mod_0_pred_60 = draw_m0_60,
                Away_mod_0_pred_60 = away_m0_60,
                Home_mod_0_pred_75 = home_m0_75,
                Draw_mod_0_pred_75 = draw_m0_75,
                Away_mod_0_pred_75 = away_m0_75,
                Home_mod_3_2015_2018_pred_0 = home_m31518_0,
                Draw_mod_3_2015_2018_pred_0 = draw_m31518_0,
                Away_mod_3_2015_2018_pred_0 = away_m31518_0,
                Home_mod_3_2015_2018_pred_15 = home_m31518_15,
                Draw_mod_3_2015_2018_pred_15 = draw_m31518_15,
                Away_mod_3_2015_2018_pred_15 = away_m31518_15,
                Home_mod_3_2015_2018_pred_30 = home_m31518_30,
                Draw_mod_3_2015_2018_pred_30 = draw_m31518_30,
                Away_mod_3_2015_2018_pred_30 = away_m31518_30,
                Home_mod_3_2015_2018_pred_45 = home_m31518_45,
                Draw_mod_3_2015_2018_pred_45 = draw_m31518_45,
                Away_mod_3_2015_2018_pred_45 = away_m31518_45,
                Home_mod_3_2015_2018_pred_60 = home_m31518_60,
                Draw_mod_3_2015_2018_pred_60 = draw_m31518_60,
                Away_mod_3_2015_2018_pred_60 = away_m31518_60,
                Home_mod_3_2015_2018_pred_75 = home_m31518_75,
                Draw_mod_3_2015_2018_pred_75 = draw_m31518_75,
                Away_mod_3_2015_2018_pred_75 = away_m31518_75)
  
  hda = cbind(tmp1, tmp2)
  HDA = rbind(HDA, hda)
}

RPS <- function(Result, pH, pD, pA) {
  verification::rps(obs = Result, pred = matrix(c(pH, pD, pA), nrow = 1))$rps
}

Brier <- function(Result, pH, pD, pA) {
  e = c(0, 0, 0)
  e[Result] = 1
  p = c(pH, pD, pA)
  sum((p - e)^2)
}

pnk <- function(Result, pH, pD, pA) {
  p = c(pH, pD, pA)
  p[Result]
}

HDA = HDA %>%
  rowwise() %>%
  mutate(RPS_mod_0_pred_0 = RPS(Result, Home_mod_0_pred_0, Draw_mod_0_pred_0, Away_mod_0_pred_0),
         RPS_mod_0_pred_15 = RPS(Result, Home_mod_0_pred_15, Draw_mod_0_pred_15, Away_mod_0_pred_15),
         RPS_mod_0_pred_30 = RPS(Result, Home_mod_0_pred_30, Draw_mod_0_pred_30, Away_mod_0_pred_30),
         RPS_mod_0_pred_45 = RPS(Result, Home_mod_0_pred_45, Draw_mod_0_pred_45, Away_mod_0_pred_45),
         RPS_mod_0_pred_60 = RPS(Result, Home_mod_0_pred_60, Draw_mod_0_pred_60, Away_mod_0_pred_60),
         RPS_mod_0_pred_75 = RPS(Result, Home_mod_0_pred_75, Draw_mod_0_pred_75, Away_mod_0_pred_75),
         RPS_mod_3_2015_2018_pred_0 = RPS(Result, Home_mod_3_2015_2018_pred_0, Draw_mod_3_2015_2018_pred_0, Away_mod_3_2015_2018_pred_0),
         RPS_mod_3_2015_2018_pred_15 = RPS(Result, Home_mod_3_2015_2018_pred_15, Draw_mod_3_2015_2018_pred_15, Away_mod_3_2015_2018_pred_15),
         RPS_mod_3_2015_2018_pred_30 = RPS(Result, Home_mod_3_2015_2018_pred_30, Draw_mod_3_2015_2018_pred_30, Away_mod_3_2015_2018_pred_30),
         RPS_mod_3_2015_2018_pred_45 = RPS(Result, Home_mod_3_2015_2018_pred_45, Draw_mod_3_2015_2018_pred_45, Away_mod_3_2015_2018_pred_45),
         RPS_mod_3_2015_2018_pred_60 = RPS(Result, Home_mod_3_2015_2018_pred_60, Draw_mod_3_2015_2018_pred_60, Away_mod_3_2015_2018_pred_60),
         RPS_mod_3_2015_2018_pred_75 = RPS(Result, Home_mod_3_2015_2018_pred_75, Draw_mod_3_2015_2018_pred_75, Away_mod_3_2015_2018_pred_75),
         
         Brier_mod_0_pred_0 = Brier(Result, Home_mod_0_pred_0, Draw_mod_0_pred_0, Away_mod_0_pred_0),
         Brier_mod_0_pred_15 = Brier(Result, Home_mod_0_pred_15, Draw_mod_0_pred_15, Away_mod_0_pred_15),
         Brier_mod_0_pred_30 = Brier(Result, Home_mod_0_pred_30, Draw_mod_0_pred_30, Away_mod_0_pred_30),
         Brier_mod_0_pred_45 = Brier(Result, Home_mod_0_pred_45, Draw_mod_0_pred_45, Away_mod_0_pred_45),
         Brier_mod_0_pred_60 = Brier(Result, Home_mod_0_pred_60, Draw_mod_0_pred_60, Away_mod_0_pred_60),
         Brier_mod_0_pred_75 = Brier(Result, Home_mod_0_pred_75, Draw_mod_0_pred_75, Away_mod_0_pred_75),
         Brier_mod_3_2015_2018_pred_0 = Brier(Result, Home_mod_3_2015_2018_pred_0, Draw_mod_3_2015_2018_pred_0, Away_mod_3_2015_2018_pred_0),
         Brier_mod_3_2015_2018_pred_15 = Brier(Result, Home_mod_3_2015_2018_pred_15, Draw_mod_3_2015_2018_pred_15, Away_mod_3_2015_2018_pred_15),
         Brier_mod_3_2015_2018_pred_30 = Brier(Result, Home_mod_3_2015_2018_pred_30, Draw_mod_3_2015_2018_pred_30, Away_mod_3_2015_2018_pred_30),
         Brier_mod_3_2015_2018_pred_45 = Brier(Result, Home_mod_3_2015_2018_pred_45, Draw_mod_3_2015_2018_pred_45, Away_mod_3_2015_2018_pred_45),
         Brier_mod_3_2015_2018_pred_60 = Brier(Result, Home_mod_3_2015_2018_pred_60, Draw_mod_3_2015_2018_pred_60, Away_mod_3_2015_2018_pred_60),
         Brier_mod_3_2015_2018_pred_75 = Brier(Result, Home_mod_3_2015_2018_pred_75, Draw_mod_3_2015_2018_pred_75, Away_mod_3_2015_2018_pred_75),
         
         pnk_mod_0_pred_0 = pnk(Result, Home_mod_0_pred_0, Draw_mod_0_pred_0, Away_mod_0_pred_0),
         pnk_mod_0_pred_15 = pnk(Result, Home_mod_0_pred_15, Draw_mod_0_pred_15, Away_mod_0_pred_15),
         pnk_mod_0_pred_30 = pnk(Result, Home_mod_0_pred_30, Draw_mod_0_pred_30, Away_mod_0_pred_30),
         pnk_mod_0_pred_45 = pnk(Result, Home_mod_0_pred_45, Draw_mod_0_pred_45, Away_mod_0_pred_45),
         pnk_mod_0_pred_60 = pnk(Result, Home_mod_0_pred_60, Draw_mod_0_pred_60, Away_mod_0_pred_60),
         pnk_mod_0_pred_75 = pnk(Result, Home_mod_0_pred_75, Draw_mod_0_pred_75, Away_mod_0_pred_75),
         pnk_mod_3_2015_2018_pred_0 = pnk(Result, Home_mod_3_2015_2018_pred_0, Draw_mod_3_2015_2018_pred_0, Away_mod_3_2015_2018_pred_0),
         pnk_mod_3_2015_2018_pred_15 = pnk(Result, Home_mod_3_2015_2018_pred_15, Draw_mod_3_2015_2018_pred_15, Away_mod_3_2015_2018_pred_15),
         pnk_mod_3_2015_2018_pred_30 = pnk(Result, Home_mod_3_2015_2018_pred_30, Draw_mod_3_2015_2018_pred_30, Away_mod_3_2015_2018_pred_30),
         pnk_mod_3_2015_2018_pred_45 = pnk(Result, Home_mod_3_2015_2018_pred_45, Draw_mod_3_2015_2018_pred_45, Away_mod_3_2015_2018_pred_45),
         pnk_mod_3_2015_2018_pred_60 = pnk(Result, Home_mod_3_2015_2018_pred_60, Draw_mod_3_2015_2018_pred_60, Away_mod_3_2015_2018_pred_60),
         pnk_mod_3_2015_2018_pred_75 = pnk(Result, Home_mod_3_2015_2018_pred_75, Draw_mod_3_2015_2018_pred_75, Away_mod_3_2015_2018_pred_75))

save(HDA, file = "pred 2019/data/HDA.RData")

