
library(dplyr)

load("pred 2020/data/predictions_mod_0.RData")
load("pred 2020/data/predictions_mod_3.RData")
load("pred 2020/data/predictions_mod_3_2019.RData")
load("pred 2020/data/predictions_mod_3_2015_2019.RData")
load("pred 2020/data/predictions_mod_3_2015_2019_g.RData")
load("pred 2020/data/predictions_mod_5_2015_2019.RData")

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
  
  home_m3_0 = predictions_mod_3[[i]]$pred_0$Result[[1]]
  draw_m3_0 = predictions_mod_3[[i]]$pred_0$Result[[2]]
  away_m3_0 = predictions_mod_3[[i]]$pred_0$Result[[3]]
  
  home_m3_15 = predictions_mod_3[[i]]$pred_15$Result[[1]]
  draw_m3_15 = predictions_mod_3[[i]]$pred_15$Result[[2]]
  away_m3_15 = predictions_mod_3[[i]]$pred_15$Result[[3]]
  
  home_m3_30 = predictions_mod_3[[i]]$pred_30$Result[[1]]
  draw_m3_30 = predictions_mod_3[[i]]$pred_30$Result[[2]]
  away_m3_30 = predictions_mod_3[[i]]$pred_30$Result[[3]]
  
  home_m3_45 = predictions_mod_3[[i]]$pred_45$Result[[1]]
  draw_m3_45 = predictions_mod_3[[i]]$pred_45$Result[[2]]
  away_m3_45 = predictions_mod_3[[i]]$pred_45$Result[[3]]
  
  home_m3_60 = predictions_mod_3[[i]]$pred_60$Result[[1]]
  draw_m3_60 = predictions_mod_3[[i]]$pred_60$Result[[2]]
  away_m3_60 = predictions_mod_3[[i]]$pred_60$Result[[3]]
  
  home_m3_75 = predictions_mod_3[[i]]$pred_75$Result[[1]]
  draw_m3_75 = predictions_mod_3[[i]]$pred_75$Result[[2]]
  away_m3_75 = predictions_mod_3[[i]]$pred_75$Result[[3]]
  
  home_m319_0 = predictions_mod_3_2019[[i]]$pred_0$Result[[1]]
  draw_m319_0 = predictions_mod_3_2019[[i]]$pred_0$Result[[2]]
  away_m319_0 = predictions_mod_3_2019[[i]]$pred_0$Result[[3]]
  
  home_m319_15 = predictions_mod_3_2019[[i]]$pred_15$Result[[1]]
  draw_m319_15 = predictions_mod_3_2019[[i]]$pred_15$Result[[2]]
  away_m319_15 = predictions_mod_3_2019[[i]]$pred_15$Result[[3]]
  
  home_m319_30 = predictions_mod_3_2019[[i]]$pred_30$Result[[1]]
  draw_m319_30 = predictions_mod_3_2019[[i]]$pred_30$Result[[2]]
  away_m319_30 = predictions_mod_3_2019[[i]]$pred_30$Result[[3]]
  
  home_m319_45 = predictions_mod_3_2019[[i]]$pred_45$Result[[1]]
  draw_m319_45 = predictions_mod_3_2019[[i]]$pred_45$Result[[2]]
  away_m319_45 = predictions_mod_3_2019[[i]]$pred_45$Result[[3]]
  
  home_m319_60 = predictions_mod_3_2019[[i]]$pred_60$Result[[1]]
  draw_m319_60 = predictions_mod_3_2019[[i]]$pred_60$Result[[2]]
  away_m319_60 = predictions_mod_3_2019[[i]]$pred_60$Result[[3]]
  
  home_m319_75 = predictions_mod_3_2019[[i]]$pred_75$Result[[1]]
  draw_m319_75 = predictions_mod_3_2019[[i]]$pred_75$Result[[2]]
  away_m319_75 = predictions_mod_3_2019[[i]]$pred_75$Result[[3]]
  
  home_m31519_0 = predictions_mod_3_2015_2019[[i]]$pred_0$Result[[1]]
  draw_m31519_0 = predictions_mod_3_2015_2019[[i]]$pred_0$Result[[2]]
  away_m31519_0 = predictions_mod_3_2015_2019[[i]]$pred_0$Result[[3]]
  
  home_m31519_15 = predictions_mod_3_2015_2019[[i]]$pred_15$Result[[1]]
  draw_m31519_15 = predictions_mod_3_2015_2019[[i]]$pred_15$Result[[2]]
  away_m31519_15 = predictions_mod_3_2015_2019[[i]]$pred_15$Result[[3]]
  
  home_m31519_30 = predictions_mod_3_2015_2019[[i]]$pred_30$Result[[1]]
  draw_m31519_30 = predictions_mod_3_2015_2019[[i]]$pred_30$Result[[2]]
  away_m31519_30 = predictions_mod_3_2015_2019[[i]]$pred_30$Result[[3]]
  
  home_m31519_45 = predictions_mod_3_2015_2019[[i]]$pred_45$Result[[1]]
  draw_m31519_45 = predictions_mod_3_2015_2019[[i]]$pred_45$Result[[2]]
  away_m31519_45 = predictions_mod_3_2015_2019[[i]]$pred_45$Result[[3]]
  
  home_m31519_60 = predictions_mod_3_2015_2019[[i]]$pred_60$Result[[1]]
  draw_m31519_60 = predictions_mod_3_2015_2019[[i]]$pred_60$Result[[2]]
  away_m31519_60 = predictions_mod_3_2015_2019[[i]]$pred_60$Result[[3]]
  
  home_m31519_75 = predictions_mod_3_2015_2019[[i]]$pred_75$Result[[1]]
  draw_m31519_75 = predictions_mod_3_2015_2019[[i]]$pred_75$Result[[2]]
  away_m31519_75 = predictions_mod_3_2015_2019[[i]]$pred_75$Result[[3]]
  
  home_m31519g_0 = predictions_mod_3_2015_2019_g[[i]]$pred_0$Result[[1]]
  draw_m31519g_0 = predictions_mod_3_2015_2019_g[[i]]$pred_0$Result[[2]]
  away_m31519g_0 = predictions_mod_3_2015_2019_g[[i]]$pred_0$Result[[3]]
  
  home_m31519g_15 = predictions_mod_3_2015_2019_g[[i]]$pred_15$Result[[1]]
  draw_m31519g_15 = predictions_mod_3_2015_2019_g[[i]]$pred_15$Result[[2]]
  away_m31519g_15 = predictions_mod_3_2015_2019_g[[i]]$pred_15$Result[[3]]
  
  home_m31519g_30 = predictions_mod_3_2015_2019_g[[i]]$pred_30$Result[[1]]
  draw_m31519g_30 = predictions_mod_3_2015_2019_g[[i]]$pred_30$Result[[2]]
  away_m31519g_30 = predictions_mod_3_2015_2019_g[[i]]$pred_30$Result[[3]]
  
  home_m31519g_45 = predictions_mod_3_2015_2019_g[[i]]$pred_45$Result[[1]]
  draw_m31519g_45 = predictions_mod_3_2015_2019_g[[i]]$pred_45$Result[[2]]
  away_m31519g_45 = predictions_mod_3_2015_2019_g[[i]]$pred_45$Result[[3]]
  
  home_m31519g_60 = predictions_mod_3_2015_2019_g[[i]]$pred_60$Result[[1]]
  draw_m31519g_60 = predictions_mod_3_2015_2019_g[[i]]$pred_60$Result[[2]]
  away_m31519g_60 = predictions_mod_3_2015_2019_g[[i]]$pred_60$Result[[3]]
  
  home_m31519g_75 = predictions_mod_3_2015_2019_g[[i]]$pred_75$Result[[1]]
  draw_m31519g_75 = predictions_mod_3_2015_2019_g[[i]]$pred_75$Result[[2]]
  away_m31519g_75 = predictions_mod_3_2015_2019_g[[i]]$pred_75$Result[[3]]
  
  home_m51519_0 = predictions_mod_5_2015_2019[[i]]$pred_0$Result[[1]]
  draw_m51519_0 = predictions_mod_5_2015_2019[[i]]$pred_0$Result[[2]]
  away_m51519_0 = predictions_mod_5_2015_2019[[i]]$pred_0$Result[[3]]
  
  home_m51519_15 = predictions_mod_5_2015_2019[[i]]$pred_15$Result[[1]]
  draw_m51519_15 = predictions_mod_5_2015_2019[[i]]$pred_15$Result[[2]]
  away_m51519_15 = predictions_mod_5_2015_2019[[i]]$pred_15$Result[[3]]
  
  home_m51519_30 = predictions_mod_5_2015_2019[[i]]$pred_30$Result[[1]]
  draw_m51519_30 = predictions_mod_5_2015_2019[[i]]$pred_30$Result[[2]]
  away_m51519_30 = predictions_mod_5_2015_2019[[i]]$pred_30$Result[[3]]
  
  home_m51519_45 = predictions_mod_5_2015_2019[[i]]$pred_45$Result[[1]]
  draw_m51519_45 = predictions_mod_5_2015_2019[[i]]$pred_45$Result[[2]]
  away_m51519_45 = predictions_mod_5_2015_2019[[i]]$pred_45$Result[[3]]
  
  home_m51519_60 = predictions_mod_5_2015_2019[[i]]$pred_60$Result[[1]]
  draw_m51519_60 = predictions_mod_5_2015_2019[[i]]$pred_60$Result[[2]]
  away_m51519_60 = predictions_mod_5_2015_2019[[i]]$pred_60$Result[[3]]
  
  home_m51519_75 = predictions_mod_5_2015_2019[[i]]$pred_75$Result[[1]]
  draw_m51519_75 = predictions_mod_5_2015_2019[[i]]$pred_75$Result[[2]]
  away_m51519_75 = predictions_mod_5_2015_2019[[i]]$pred_75$Result[[3]]
  
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
                Home_mod_3_pred_0 = home_m3_0,
                Draw_mod_3_pred_0 = draw_m3_0,
                Away_mod_3_pred_0 = away_m3_0,
                Home_mod_3_pred_15 = home_m3_15,
                Draw_mod_3_pred_15 = draw_m3_15,
                Away_mod_3_pred_15 = away_m3_15,
                Home_mod_3_pred_30 = home_m3_30,
                Draw_mod_3_pred_30 = draw_m3_30,
                Away_mod_3_pred_30 = away_m3_30,
                Home_mod_3_pred_45 = home_m3_45,
                Draw_mod_3_pred_45 = draw_m3_45,
                Away_mod_3_pred_45 = away_m3_45,
                Home_mod_3_pred_60 = home_m3_60,
                Draw_mod_3_pred_60 = draw_m3_60,
                Away_mod_3_pred_60 = away_m3_60,
                Home_mod_3_pred_75 = home_m3_75,
                Draw_mod_3_pred_75 = draw_m3_75,
                Away_mod_3_pred_75 = away_m3_75,
                Home_mod_3_2019_pred_0 = home_m319_0,
                Draw_mod_3_2019_pred_0 = draw_m319_0,
                Away_mod_3_2019_pred_0 = away_m319_0,
                Home_mod_3_2019_pred_15 = home_m319_15,
                Draw_mod_3_2019_pred_15 = draw_m319_15,
                Away_mod_3_2019_pred_15 = away_m319_15,
                Home_mod_3_2019_pred_30 = home_m319_30,
                Draw_mod_3_2019_pred_30 = draw_m319_30,
                Away_mod_3_2019_pred_30 = away_m319_30,
                Home_mod_3_2019_pred_45 = home_m319_45,
                Draw_mod_3_2019_pred_45 = draw_m319_45,
                Away_mod_3_2019_pred_45 = away_m319_45,
                Home_mod_3_2019_pred_60 = home_m319_60,
                Draw_mod_3_2019_pred_60 = draw_m319_60,
                Away_mod_3_2019_pred_60 = away_m319_60,
                Home_mod_3_2019_pred_75 = home_m319_75,
                Draw_mod_3_2019_pred_75 = draw_m319_75,
                Away_mod_3_2019_pred_75 = away_m319_75,
                Home_mod_3_2015_2019_pred_0 = home_m31519_0,
                Draw_mod_3_2015_2019_pred_0 = draw_m31519_0,
                Away_mod_3_2015_2019_pred_0 = away_m31519_0,
                Home_mod_3_2015_2019_pred_15 = home_m31519_15,
                Draw_mod_3_2015_2019_pred_15 = draw_m31519_15,
                Away_mod_3_2015_2019_pred_15 = away_m31519_15,
                Home_mod_3_2015_2019_pred_30 = home_m31519_30,
                Draw_mod_3_2015_2019_pred_30 = draw_m31519_30,
                Away_mod_3_2015_2019_pred_30 = away_m31519_30,
                Home_mod_3_2015_2019_pred_45 = home_m31519_45,
                Draw_mod_3_2015_2019_pred_45 = draw_m31519_45,
                Away_mod_3_2015_2019_pred_45 = away_m31519_45,
                Home_mod_3_2015_2019_pred_60 = home_m31519_60,
                Draw_mod_3_2015_2019_pred_60 = draw_m31519_60,
                Away_mod_3_2015_2019_pred_60 = away_m31519_60,
                Home_mod_3_2015_2019_pred_75 = home_m31519_75,
                Draw_mod_3_2015_2019_pred_75 = draw_m31519_75,
                Away_mod_3_2015_2019_pred_75 = away_m31519_75,
                Home_mod_3_2015_2019_g_pred_0 = home_m31519g_0,
                Draw_mod_3_2015_2019_g_pred_0 = draw_m31519g_0,
                Away_mod_3_2015_2019_g_pred_0 = away_m31519g_0,
                Home_mod_3_2015_2019_g_pred_15 = home_m31519g_15,
                Draw_mod_3_2015_2019_g_pred_15 = draw_m31519g_15,
                Away_mod_3_2015_2019_g_pred_15 = away_m31519g_15,
                Home_mod_3_2015_2019_g_pred_30 = home_m31519g_30,
                Draw_mod_3_2015_2019_g_pred_30 = draw_m31519g_30,
                Away_mod_3_2015_2019_g_pred_30 = away_m31519g_30,
                Home_mod_3_2015_2019_g_pred_45 = home_m31519g_45,
                Draw_mod_3_2015_2019_g_pred_45 = draw_m31519g_45,
                Away_mod_3_2015_2019_g_pred_45 = away_m31519g_45,
                Home_mod_3_2015_2019_g_pred_60 = home_m31519g_60,
                Draw_mod_3_2015_2019_g_pred_60 = draw_m31519g_60,
                Away_mod_3_2015_2019_g_pred_60 = away_m31519g_60,
                Home_mod_3_2015_2019_g_pred_75 = home_m31519g_75,
                Draw_mod_3_2015_2019_g_pred_75 = draw_m31519g_75,
                Away_mod_3_2015_2019_g_pred_75 = away_m31519g_75,
                Home_mod_5_2015_2019_pred_0 = home_m51519_0,
                Draw_mod_5_2015_2019_pred_0 = draw_m51519_0,
                Away_mod_5_2015_2019_pred_0 = away_m51519_0,
                Home_mod_5_2015_2019_pred_15 = home_m51519_15,
                Draw_mod_5_2015_2019_pred_15 = draw_m51519_15,
                Away_mod_5_2015_2019_pred_15 = away_m51519_15,
                Home_mod_5_2015_2019_pred_30 = home_m51519_30,
                Draw_mod_5_2015_2019_pred_30 = draw_m51519_30,
                Away_mod_5_2015_2019_pred_30 = away_m51519_30,
                Home_mod_5_2015_2019_pred_45 = home_m51519_45,
                Draw_mod_5_2015_2019_pred_45 = draw_m51519_45,
                Away_mod_5_2015_2019_pred_45 = away_m51519_45,
                Home_mod_5_2015_2019_pred_60 = home_m51519_60,
                Draw_mod_5_2015_2019_pred_60 = draw_m51519_60,
                Away_mod_5_2015_2019_pred_60 = away_m51519_60,
                Home_mod_5_2015_2019_pred_75 = home_m51519_75,
                Draw_mod_5_2015_2019_pred_75 = draw_m51519_75,
                Away_mod_5_2015_2019_pred_75 = away_m51519_75)
  
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
         RPS_mod_3_pred_0 = RPS(Result, Home_mod_3_pred_0, Draw_mod_3_pred_0, Away_mod_3_pred_0),
         RPS_mod_3_pred_15 = RPS(Result, Home_mod_3_pred_15, Draw_mod_3_pred_15, Away_mod_3_pred_15),
         RPS_mod_3_pred_30 = RPS(Result, Home_mod_3_pred_30, Draw_mod_3_pred_30, Away_mod_3_pred_30),
         RPS_mod_3_pred_45 = RPS(Result, Home_mod_3_pred_45, Draw_mod_3_pred_45, Away_mod_3_pred_45),
         RPS_mod_3_pred_60 = RPS(Result, Home_mod_3_pred_60, Draw_mod_3_pred_60, Away_mod_3_pred_60),
         RPS_mod_3_pred_75 = RPS(Result, Home_mod_3_pred_75, Draw_mod_3_pred_75, Away_mod_3_pred_75),
         RPS_mod_3_2019_pred_0 = RPS(Result, Home_mod_3_2019_pred_0, Draw_mod_3_2019_pred_0, Away_mod_3_2019_pred_0),
         RPS_mod_3_2019_pred_15 = RPS(Result, Home_mod_3_2019_pred_15, Draw_mod_3_2019_pred_15, Away_mod_3_2019_pred_15),
         RPS_mod_3_2019_pred_30 = RPS(Result, Home_mod_3_2019_pred_30, Draw_mod_3_2019_pred_30, Away_mod_3_2019_pred_30),
         RPS_mod_3_2019_pred_45 = RPS(Result, Home_mod_3_2019_pred_45, Draw_mod_3_2019_pred_45, Away_mod_3_2019_pred_45),
         RPS_mod_3_2019_pred_60 = RPS(Result, Home_mod_3_2019_pred_60, Draw_mod_3_2019_pred_60, Away_mod_3_2019_pred_60),
         RPS_mod_3_2019_pred_75 = RPS(Result, Home_mod_3_2019_pred_75, Draw_mod_3_2019_pred_75, Away_mod_3_2019_pred_75),
         RPS_mod_3_2015_2019_pred_0 = RPS(Result, Home_mod_3_2015_2019_pred_0, Draw_mod_3_2015_2019_pred_0, Away_mod_3_2015_2019_pred_0),
         RPS_mod_3_2015_2019_pred_15 = RPS(Result, Home_mod_3_2015_2019_pred_15, Draw_mod_3_2015_2019_pred_15, Away_mod_3_2015_2019_pred_15),
         RPS_mod_3_2015_2019_pred_30 = RPS(Result, Home_mod_3_2015_2019_pred_30, Draw_mod_3_2015_2019_pred_30, Away_mod_3_2015_2019_pred_30),
         RPS_mod_3_2015_2019_pred_45 = RPS(Result, Home_mod_3_2015_2019_pred_45, Draw_mod_3_2015_2019_pred_45, Away_mod_3_2015_2019_pred_45),
         RPS_mod_3_2015_2019_pred_60 = RPS(Result, Home_mod_3_2015_2019_pred_60, Draw_mod_3_2015_2019_pred_60, Away_mod_3_2015_2019_pred_60),
         RPS_mod_3_2015_2019_pred_75 = RPS(Result, Home_mod_3_2015_2019_pred_75, Draw_mod_3_2015_2019_pred_75, Away_mod_3_2015_2019_pred_75),
         RPS_mod_3_2015_2019_g_pred_0 = RPS(Result, Home_mod_3_2015_2019_g_pred_0, Draw_mod_3_2015_2019_g_pred_0, Away_mod_3_2015_2019_g_pred_0),
         RPS_mod_3_2015_2019_g_pred_15 = RPS(Result, Home_mod_3_2015_2019_g_pred_15, Draw_mod_3_2015_2019_g_pred_15, Away_mod_3_2015_2019_g_pred_15),
         RPS_mod_3_2015_2019_g_pred_30 = RPS(Result, Home_mod_3_2015_2019_g_pred_30, Draw_mod_3_2015_2019_g_pred_30, Away_mod_3_2015_2019_g_pred_30),
         RPS_mod_3_2015_2019_g_pred_45 = RPS(Result, Home_mod_3_2015_2019_g_pred_45, Draw_mod_3_2015_2019_g_pred_45, Away_mod_3_2015_2019_g_pred_45),
         RPS_mod_3_2015_2019_g_pred_60 = RPS(Result, Home_mod_3_2015_2019_g_pred_60, Draw_mod_3_2015_2019_g_pred_60, Away_mod_3_2015_2019_g_pred_60),
         RPS_mod_3_2015_2019_g_pred_75 = RPS(Result, Home_mod_3_2015_2019_g_pred_75, Draw_mod_3_2015_2019_g_pred_75, Away_mod_3_2015_2019_g_pred_75),
         RPS_mod_5_2015_2019_pred_0 = RPS(Result, Home_mod_5_2015_2019_pred_0, Draw_mod_5_2015_2019_pred_0, Away_mod_5_2015_2019_pred_0),
         RPS_mod_5_2015_2019_pred_15 = RPS(Result, Home_mod_5_2015_2019_pred_15, Draw_mod_5_2015_2019_pred_15, Away_mod_5_2015_2019_pred_15),
         RPS_mod_5_2015_2019_pred_30 = RPS(Result, Home_mod_5_2015_2019_pred_30, Draw_mod_5_2015_2019_pred_30, Away_mod_5_2015_2019_pred_30),
         RPS_mod_5_2015_2019_pred_45 = RPS(Result, Home_mod_5_2015_2019_pred_45, Draw_mod_5_2015_2019_pred_45, Away_mod_5_2015_2019_pred_45),
         RPS_mod_5_2015_2019_pred_60 = RPS(Result, Home_mod_5_2015_2019_pred_60, Draw_mod_5_2015_2019_pred_60, Away_mod_5_2015_2019_pred_60),
         RPS_mod_5_2015_2019_pred_75 = RPS(Result, Home_mod_5_2015_2019_pred_75, Draw_mod_5_2015_2019_pred_75, Away_mod_5_2015_2019_pred_75),
         
         Brier_mod_0_pred_0 = Brier(Result, Home_mod_0_pred_0, Draw_mod_0_pred_0, Away_mod_0_pred_0),
         Brier_mod_0_pred_15 = Brier(Result, Home_mod_0_pred_15, Draw_mod_0_pred_15, Away_mod_0_pred_15),
         Brier_mod_0_pred_30 = Brier(Result, Home_mod_0_pred_30, Draw_mod_0_pred_30, Away_mod_0_pred_30),
         Brier_mod_0_pred_45 = Brier(Result, Home_mod_0_pred_45, Draw_mod_0_pred_45, Away_mod_0_pred_45),
         Brier_mod_0_pred_60 = Brier(Result, Home_mod_0_pred_60, Draw_mod_0_pred_60, Away_mod_0_pred_60),
         Brier_mod_0_pred_75 = Brier(Result, Home_mod_0_pred_75, Draw_mod_0_pred_75, Away_mod_0_pred_75),
         Brier_mod_3_pred_0 = Brier(Result, Home_mod_3_pred_0, Draw_mod_3_pred_0, Away_mod_3_pred_0),
         Brier_mod_3_pred_15 = Brier(Result, Home_mod_3_pred_15, Draw_mod_3_pred_15, Away_mod_3_pred_15),
         Brier_mod_3_pred_30 = Brier(Result, Home_mod_3_pred_30, Draw_mod_3_pred_30, Away_mod_3_pred_30),
         Brier_mod_3_pred_45 = Brier(Result, Home_mod_3_pred_45, Draw_mod_3_pred_45, Away_mod_3_pred_45),
         Brier_mod_3_pred_60 = Brier(Result, Home_mod_3_pred_60, Draw_mod_3_pred_60, Away_mod_3_pred_60),
         Brier_mod_3_pred_75 = Brier(Result, Home_mod_3_pred_75, Draw_mod_3_pred_75, Away_mod_3_pred_75),
         Brier_mod_3_2019_pred_0 = Brier(Result, Home_mod_3_2019_pred_0, Draw_mod_3_2019_pred_0, Away_mod_3_2019_pred_0),
         Brier_mod_3_2019_pred_15 = Brier(Result, Home_mod_3_2019_pred_15, Draw_mod_3_2019_pred_15, Away_mod_3_2019_pred_15),
         Brier_mod_3_2019_pred_30 = Brier(Result, Home_mod_3_2019_pred_30, Draw_mod_3_2019_pred_30, Away_mod_3_2019_pred_30),
         Brier_mod_3_2019_pred_45 = Brier(Result, Home_mod_3_2019_pred_45, Draw_mod_3_2019_pred_45, Away_mod_3_2019_pred_45),
         Brier_mod_3_2019_pred_60 = Brier(Result, Home_mod_3_2019_pred_60, Draw_mod_3_2019_pred_60, Away_mod_3_2019_pred_60),
         Brier_mod_3_2019_pred_75 = Brier(Result, Home_mod_3_2019_pred_75, Draw_mod_3_2019_pred_75, Away_mod_3_2019_pred_75),
         Brier_mod_3_2015_2019_pred_0 = Brier(Result, Home_mod_3_2015_2019_pred_0, Draw_mod_3_2015_2019_pred_0, Away_mod_3_2015_2019_pred_0),
         Brier_mod_3_2015_2019_pred_15 = Brier(Result, Home_mod_3_2015_2019_pred_15, Draw_mod_3_2015_2019_pred_15, Away_mod_3_2015_2019_pred_15),
         Brier_mod_3_2015_2019_pred_30 = Brier(Result, Home_mod_3_2015_2019_pred_30, Draw_mod_3_2015_2019_pred_30, Away_mod_3_2015_2019_pred_30),
         Brier_mod_3_2015_2019_pred_45 = Brier(Result, Home_mod_3_2015_2019_pred_45, Draw_mod_3_2015_2019_pred_45, Away_mod_3_2015_2019_pred_45),
         Brier_mod_3_2015_2019_pred_60 = Brier(Result, Home_mod_3_2015_2019_pred_60, Draw_mod_3_2015_2019_pred_60, Away_mod_3_2015_2019_pred_60),
         Brier_mod_3_2015_2019_pred_75 = Brier(Result, Home_mod_3_2015_2019_pred_75, Draw_mod_3_2015_2019_pred_75, Away_mod_3_2015_2019_pred_75),
         Brier_mod_3_2015_2019_g_pred_0 = Brier(Result, Home_mod_3_2015_2019_g_pred_0, Draw_mod_3_2015_2019_g_pred_0, Away_mod_3_2015_2019_g_pred_0),
         Brier_mod_3_2015_2019_g_pred_15 = Brier(Result, Home_mod_3_2015_2019_g_pred_15, Draw_mod_3_2015_2019_g_pred_15, Away_mod_3_2015_2019_g_pred_15),
         Brier_mod_3_2015_2019_g_pred_30 = Brier(Result, Home_mod_3_2015_2019_g_pred_30, Draw_mod_3_2015_2019_g_pred_30, Away_mod_3_2015_2019_g_pred_30),
         Brier_mod_3_2015_2019_g_pred_45 = Brier(Result, Home_mod_3_2015_2019_g_pred_45, Draw_mod_3_2015_2019_g_pred_45, Away_mod_3_2015_2019_g_pred_45),
         Brier_mod_3_2015_2019_g_pred_60 = Brier(Result, Home_mod_3_2015_2019_g_pred_60, Draw_mod_3_2015_2019_g_pred_60, Away_mod_3_2015_2019_g_pred_60),
         Brier_mod_3_2015_2019_g_pred_75 = Brier(Result, Home_mod_3_2015_2019_g_pred_75, Draw_mod_3_2015_2019_g_pred_75, Away_mod_3_2015_2019_g_pred_75),
         Brier_mod_5_2015_2019_pred_0 = Brier(Result, Home_mod_5_2015_2019_pred_0, Draw_mod_5_2015_2019_pred_0, Away_mod_5_2015_2019_pred_0),
         Brier_mod_5_2015_2019_pred_15 = Brier(Result, Home_mod_5_2015_2019_pred_15, Draw_mod_5_2015_2019_pred_15, Away_mod_5_2015_2019_pred_15),
         Brier_mod_5_2015_2019_pred_30 = Brier(Result, Home_mod_5_2015_2019_pred_30, Draw_mod_5_2015_2019_pred_30, Away_mod_5_2015_2019_pred_30),
         Brier_mod_5_2015_2019_pred_45 = Brier(Result, Home_mod_5_2015_2019_pred_45, Draw_mod_5_2015_2019_pred_45, Away_mod_5_2015_2019_pred_45),
         Brier_mod_5_2015_2019_pred_60 = Brier(Result, Home_mod_5_2015_2019_pred_60, Draw_mod_5_2015_2019_pred_60, Away_mod_5_2015_2019_pred_60),
         Brier_mod_5_2015_2019_pred_75 = Brier(Result, Home_mod_5_2015_2019_pred_75, Draw_mod_5_2015_2019_pred_75, Away_mod_5_2015_2019_pred_75),
         
         pnk_mod_0_pred_0 = pnk(Result, Home_mod_0_pred_0, Draw_mod_0_pred_0, Away_mod_0_pred_0),
         pnk_mod_0_pred_15 = pnk(Result, Home_mod_0_pred_15, Draw_mod_0_pred_15, Away_mod_0_pred_15),
         pnk_mod_0_pred_30 = pnk(Result, Home_mod_0_pred_30, Draw_mod_0_pred_30, Away_mod_0_pred_30),
         pnk_mod_0_pred_45 = pnk(Result, Home_mod_0_pred_45, Draw_mod_0_pred_45, Away_mod_0_pred_45),
         pnk_mod_0_pred_60 = pnk(Result, Home_mod_0_pred_60, Draw_mod_0_pred_60, Away_mod_0_pred_60),
         pnk_mod_0_pred_75 = pnk(Result, Home_mod_0_pred_75, Draw_mod_0_pred_75, Away_mod_0_pred_75),
         pnk_mod_3_pred_0 = pnk(Result, Home_mod_3_pred_0, Draw_mod_3_pred_0, Away_mod_3_pred_0),
         pnk_mod_3_pred_15 = pnk(Result, Home_mod_3_pred_15, Draw_mod_3_pred_15, Away_mod_3_pred_15),
         pnk_mod_3_pred_30 = pnk(Result, Home_mod_3_pred_30, Draw_mod_3_pred_30, Away_mod_3_pred_30),
         pnk_mod_3_pred_45 = pnk(Result, Home_mod_3_pred_45, Draw_mod_3_pred_45, Away_mod_3_pred_45),
         pnk_mod_3_pred_60 = pnk(Result, Home_mod_3_pred_60, Draw_mod_3_pred_60, Away_mod_3_pred_60),
         pnk_mod_3_pred_75 = pnk(Result, Home_mod_3_pred_75, Draw_mod_3_pred_75, Away_mod_3_pred_75),
         pnk_mod_3_2019_pred_0 = pnk(Result, Home_mod_3_2019_pred_0, Draw_mod_3_2019_pred_0, Away_mod_3_2019_pred_0),
         pnk_mod_3_2019_pred_15 = pnk(Result, Home_mod_3_2019_pred_15, Draw_mod_3_2019_pred_15, Away_mod_3_2019_pred_15),
         pnk_mod_3_2019_pred_30 = pnk(Result, Home_mod_3_2019_pred_30, Draw_mod_3_2019_pred_30, Away_mod_3_2019_pred_30),
         pnk_mod_3_2019_pred_45 = pnk(Result, Home_mod_3_2019_pred_45, Draw_mod_3_2019_pred_45, Away_mod_3_2019_pred_45),
         pnk_mod_3_2019_pred_60 = pnk(Result, Home_mod_3_2019_pred_60, Draw_mod_3_2019_pred_60, Away_mod_3_2019_pred_60),
         pnk_mod_3_2019_pred_75 = pnk(Result, Home_mod_3_2019_pred_75, Draw_mod_3_2019_pred_75, Away_mod_3_2019_pred_75),
         pnk_mod_3_2015_2019_pred_0 = pnk(Result, Home_mod_3_2015_2019_pred_0, Draw_mod_3_2015_2019_pred_0, Away_mod_3_2015_2019_pred_0),
         pnk_mod_3_2015_2019_pred_15 = pnk(Result, Home_mod_3_2015_2019_pred_15, Draw_mod_3_2015_2019_pred_15, Away_mod_3_2015_2019_pred_15),
         pnk_mod_3_2015_2019_pred_30 = pnk(Result, Home_mod_3_2015_2019_pred_30, Draw_mod_3_2015_2019_pred_30, Away_mod_3_2015_2019_pred_30),
         pnk_mod_3_2015_2019_pred_45 = pnk(Result, Home_mod_3_2015_2019_pred_45, Draw_mod_3_2015_2019_pred_45, Away_mod_3_2015_2019_pred_45),
         pnk_mod_3_2015_2019_pred_60 = pnk(Result, Home_mod_3_2015_2019_pred_60, Draw_mod_3_2015_2019_pred_60, Away_mod_3_2015_2019_pred_60),
         pnk_mod_3_2015_2019_pred_75 = pnk(Result, Home_mod_3_2015_2019_pred_75, Draw_mod_3_2015_2019_pred_75, Away_mod_3_2015_2019_pred_75),
         pnk_mod_3_2015_2019_g_pred_0 = pnk(Result, Home_mod_3_2015_2019_g_pred_0, Draw_mod_3_2015_2019_g_pred_0, Away_mod_3_2015_2019_g_pred_0),
         pnk_mod_3_2015_2019_g_pred_15 = pnk(Result, Home_mod_3_2015_2019_g_pred_15, Draw_mod_3_2015_2019_g_pred_15, Away_mod_3_2015_2019_g_pred_15),
         pnk_mod_3_2015_2019_g_pred_30 = pnk(Result, Home_mod_3_2015_2019_g_pred_30, Draw_mod_3_2015_2019_g_pred_30, Away_mod_3_2015_2019_g_pred_30),
         pnk_mod_3_2015_2019_g_pred_45 = pnk(Result, Home_mod_3_2015_2019_g_pred_45, Draw_mod_3_2015_2019_g_pred_45, Away_mod_3_2015_2019_g_pred_45),
         pnk_mod_3_2015_2019_g_pred_60 = pnk(Result, Home_mod_3_2015_2019_g_pred_60, Draw_mod_3_2015_2019_g_pred_60, Away_mod_3_2015_2019_g_pred_60),
         pnk_mod_3_2015_2019_g_pred_75 = pnk(Result, Home_mod_3_2015_2019_g_pred_75, Draw_mod_3_2015_2019_g_pred_75, Away_mod_3_2015_2019_g_pred_75),
         pnk_mod_5_2015_2019_pred_0 = pnk(Result, Home_mod_5_2015_2019_pred_0, Draw_mod_5_2015_2019_pred_0, Away_mod_5_2015_2019_pred_0),
         pnk_mod_5_2015_2019_pred_15 = pnk(Result, Home_mod_5_2015_2019_pred_15, Draw_mod_5_2015_2019_pred_15, Away_mod_5_2015_2019_pred_15),
         pnk_mod_5_2015_2019_pred_30 = pnk(Result, Home_mod_5_2015_2019_pred_30, Draw_mod_5_2015_2019_pred_30, Away_mod_5_2015_2019_pred_30),
         pnk_mod_5_2015_2019_pred_45 = pnk(Result, Home_mod_5_2015_2019_pred_45, Draw_mod_5_2015_2019_pred_45, Away_mod_5_2015_2019_pred_45),
         pnk_mod_5_2015_2019_pred_60 = pnk(Result, Home_mod_5_2015_2019_pred_60, Draw_mod_5_2015_2019_pred_60, Away_mod_5_2015_2019_pred_60),
         pnk_mod_5_2015_2019_pred_75 = pnk(Result, Home_mod_5_2015_2019_pred_75, Draw_mod_5_2015_2019_pred_75, Away_mod_5_2015_2019_pred_75))

save(HDA, file = "pred 2020/data/HDA.RData")

