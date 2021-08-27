
library(dplyr)

load("weight/data/predictions_mod_0_dc_v2.RData")
load("weight/data/predictions_mod_B_dc.RData")
load("weight/data/predictions_mod_C_dc.RData")
load("weight/data/predictions_mod_3_dc.RData")
load("weight/data/predictions_mod_12_dc.RData")

HDA_dc = tibble()

for(i in 1:length(predictions_mod_0_dc)) {
  
  tmp1 = predictions_mod_0_dc[[i]]$Match %>%
    select(-Stoppage_Time_1, -Stoppage_Time_2) %>%
    mutate(Result = ifelse(Score_Home > Score_Away, 1,
                           ifelse(Score_Home == Score_Away, 2, 
                                  3)))
  
  Home_mod_0_pred_0 = predictions_mod_0_dc[[i]]$pred_0$Result[[1]]
  Draw_mod_0_pred_0 = predictions_mod_0_dc[[i]]$pred_0$Result[[2]]
  Away_mod_0_pred_0 = predictions_mod_0_dc[[i]]$pred_0$Result[[3]]
  
  Home_mod_0_pred_15 = predictions_mod_0_dc[[i]]$pred_15$Result[[1]]
  Draw_mod_0_pred_15 = predictions_mod_0_dc[[i]]$pred_15$Result[[2]]
  Away_mod_0_pred_15 = predictions_mod_0_dc[[i]]$pred_15$Result[[3]]
  
  Home_mod_0_pred_30 = predictions_mod_0_dc[[i]]$pred_30$Result[[1]]
  Draw_mod_0_pred_30 = predictions_mod_0_dc[[i]]$pred_30$Result[[2]]
  Away_mod_0_pred_30 = predictions_mod_0_dc[[i]]$pred_30$Result[[3]]
  
  Home_mod_0_pred_45 = predictions_mod_0_dc[[i]]$pred_45$Result[[1]]
  Draw_mod_0_pred_45 = predictions_mod_0_dc[[i]]$pred_45$Result[[2]]
  Away_mod_0_pred_45 = predictions_mod_0_dc[[i]]$pred_45$Result[[3]]
  
  Home_mod_0_pred_60 = predictions_mod_0_dc[[i]]$pred_60$Result[[1]]
  Draw_mod_0_pred_60 = predictions_mod_0_dc[[i]]$pred_60$Result[[2]]
  Away_mod_0_pred_60 = predictions_mod_0_dc[[i]]$pred_60$Result[[3]]
  
  Home_mod_0_pred_75 = predictions_mod_0_dc[[i]]$pred_75$Result[[1]]
  Draw_mod_0_pred_75 = predictions_mod_0_dc[[i]]$pred_75$Result[[2]]
  Away_mod_0_pred_75 = predictions_mod_0_dc[[i]]$pred_75$Result[[3]]
  
  Home_mod_B_pred_0 = predictions_mod_B_dc[[i]]$pred_0$Result[[1]]
  Draw_mod_B_pred_0 = predictions_mod_B_dc[[i]]$pred_0$Result[[2]]
  Away_mod_B_pred_0 = predictions_mod_B_dc[[i]]$pred_0$Result[[3]]
  
  Home_mod_B_pred_15 = predictions_mod_B_dc[[i]]$pred_15$Result[[1]]
  Draw_mod_B_pred_15 = predictions_mod_B_dc[[i]]$pred_15$Result[[2]]
  Away_mod_B_pred_15 = predictions_mod_B_dc[[i]]$pred_15$Result[[3]]
  
  Home_mod_B_pred_30 = predictions_mod_B_dc[[i]]$pred_30$Result[[1]]
  Draw_mod_B_pred_30 = predictions_mod_B_dc[[i]]$pred_30$Result[[2]]
  Away_mod_B_pred_30 = predictions_mod_B_dc[[i]]$pred_30$Result[[3]]
  
  Home_mod_B_pred_45 = predictions_mod_B_dc[[i]]$pred_45$Result[[1]]
  Draw_mod_B_pred_45 = predictions_mod_B_dc[[i]]$pred_45$Result[[2]]
  Away_mod_B_pred_45 = predictions_mod_B_dc[[i]]$pred_45$Result[[3]]
  
  Home_mod_B_pred_60 = predictions_mod_B_dc[[i]]$pred_60$Result[[1]]
  Draw_mod_B_pred_60 = predictions_mod_B_dc[[i]]$pred_60$Result[[2]]
  Away_mod_B_pred_60 = predictions_mod_B_dc[[i]]$pred_60$Result[[3]]
  
  Home_mod_B_pred_75 = predictions_mod_B_dc[[i]]$pred_75$Result[[1]]
  Draw_mod_B_pred_75 = predictions_mod_B_dc[[i]]$pred_75$Result[[2]]
  Away_mod_B_pred_75 = predictions_mod_B_dc[[i]]$pred_75$Result[[3]]
  
  Home_mod_C_pred_0 = predictions_mod_C_dc[[i]]$pred_0$Result[[1]]
  Draw_mod_C_pred_0 = predictions_mod_C_dc[[i]]$pred_0$Result[[2]]
  Away_mod_C_pred_0 = predictions_mod_C_dc[[i]]$pred_0$Result[[3]]
  
  Home_mod_C_pred_15 = predictions_mod_C_dc[[i]]$pred_15$Result[[1]]
  Draw_mod_C_pred_15 = predictions_mod_C_dc[[i]]$pred_15$Result[[2]]
  Away_mod_C_pred_15 = predictions_mod_C_dc[[i]]$pred_15$Result[[3]]
  
  Home_mod_C_pred_30 = predictions_mod_C_dc[[i]]$pred_30$Result[[1]]
  Draw_mod_C_pred_30 = predictions_mod_C_dc[[i]]$pred_30$Result[[2]]
  Away_mod_C_pred_30 = predictions_mod_C_dc[[i]]$pred_30$Result[[3]]
  
  Home_mod_C_pred_45 = predictions_mod_C_dc[[i]]$pred_45$Result[[1]]
  Draw_mod_C_pred_45 = predictions_mod_C_dc[[i]]$pred_45$Result[[2]]
  Away_mod_C_pred_45 = predictions_mod_C_dc[[i]]$pred_45$Result[[3]]
  
  Home_mod_C_pred_60 = predictions_mod_C_dc[[i]]$pred_60$Result[[1]]
  Draw_mod_C_pred_60 = predictions_mod_C_dc[[i]]$pred_60$Result[[2]]
  Away_mod_C_pred_60 = predictions_mod_C_dc[[i]]$pred_60$Result[[3]]
  
  Home_mod_C_pred_75 = predictions_mod_C_dc[[i]]$pred_75$Result[[1]]
  Draw_mod_C_pred_75 = predictions_mod_C_dc[[i]]$pred_75$Result[[2]]
  Away_mod_C_pred_75 = predictions_mod_C_dc[[i]]$pred_75$Result[[3]]
  
  Home_mod_3_pred_0 = predictions_mod_3_dc[[i]]$pred_0$Result[[1]]
  Draw_mod_3_pred_0 = predictions_mod_3_dc[[i]]$pred_0$Result[[2]]
  Away_mod_3_pred_0 = predictions_mod_3_dc[[i]]$pred_0$Result[[3]]
  
  Home_mod_3_pred_15 = predictions_mod_3_dc[[i]]$pred_15$Result[[1]]
  Draw_mod_3_pred_15 = predictions_mod_3_dc[[i]]$pred_15$Result[[2]]
  Away_mod_3_pred_15 = predictions_mod_3_dc[[i]]$pred_15$Result[[3]]
  
  Home_mod_3_pred_30 = predictions_mod_3_dc[[i]]$pred_30$Result[[1]]
  Draw_mod_3_pred_30 = predictions_mod_3_dc[[i]]$pred_30$Result[[2]]
  Away_mod_3_pred_30 = predictions_mod_3_dc[[i]]$pred_30$Result[[3]]
  
  Home_mod_3_pred_45 = predictions_mod_3_dc[[i]]$pred_45$Result[[1]]
  Draw_mod_3_pred_45 = predictions_mod_3_dc[[i]]$pred_45$Result[[2]]
  Away_mod_3_pred_45 = predictions_mod_3_dc[[i]]$pred_45$Result[[3]]
  
  Home_mod_3_pred_60 = predictions_mod_3_dc[[i]]$pred_60$Result[[1]]
  Draw_mod_3_pred_60 = predictions_mod_3_dc[[i]]$pred_60$Result[[2]]
  Away_mod_3_pred_60 = predictions_mod_3_dc[[i]]$pred_60$Result[[3]]
  
  Home_mod_3_pred_75 = predictions_mod_3_dc[[i]]$pred_75$Result[[1]]
  Draw_mod_3_pred_75 = predictions_mod_3_dc[[i]]$pred_75$Result[[2]]
  Away_mod_3_pred_75 = predictions_mod_3_dc[[i]]$pred_75$Result[[3]]
  
  Home_mod_12_pred_0 = predictions_mod_12_dc[[i]]$pred_0$Result[[1]]
  Draw_mod_12_pred_0 = predictions_mod_12_dc[[i]]$pred_0$Result[[2]]
  Away_mod_12_pred_0 = predictions_mod_12_dc[[i]]$pred_0$Result[[3]]
  
  Home_mod_12_pred_15 = predictions_mod_12_dc[[i]]$pred_15$Result[[1]]
  Draw_mod_12_pred_15 = predictions_mod_12_dc[[i]]$pred_15$Result[[2]]
  Away_mod_12_pred_15 = predictions_mod_12_dc[[i]]$pred_15$Result[[3]]
  
  Home_mod_12_pred_30 = predictions_mod_12_dc[[i]]$pred_30$Result[[1]]
  Draw_mod_12_pred_30 = predictions_mod_12_dc[[i]]$pred_30$Result[[2]]
  Away_mod_12_pred_30 = predictions_mod_12_dc[[i]]$pred_30$Result[[3]]
  
  Home_mod_12_pred_45 = predictions_mod_12_dc[[i]]$pred_45$Result[[1]]
  Draw_mod_12_pred_45 = predictions_mod_12_dc[[i]]$pred_45$Result[[2]]
  Away_mod_12_pred_45 = predictions_mod_12_dc[[i]]$pred_45$Result[[3]]
  
  Home_mod_12_pred_60 = predictions_mod_12_dc[[i]]$pred_60$Result[[1]]
  Draw_mod_12_pred_60 = predictions_mod_12_dc[[i]]$pred_60$Result[[2]]
  Away_mod_12_pred_60 = predictions_mod_12_dc[[i]]$pred_60$Result[[3]]
  
  Home_mod_12_pred_75 = predictions_mod_12_dc[[i]]$pred_75$Result[[1]]
  Draw_mod_12_pred_75 = predictions_mod_12_dc[[i]]$pred_75$Result[[2]]
  Away_mod_12_pred_75 = predictions_mod_12_dc[[i]]$pred_75$Result[[3]]
  
  tmp2 = tibble(Home_mod_0_pred_0, Draw_mod_0_pred_0, Away_mod_0_pred_0,
                Home_mod_0_pred_15, Draw_mod_0_pred_15, Away_mod_0_pred_15,
                Home_mod_0_pred_30, Draw_mod_0_pred_30, Away_mod_0_pred_30,
                Home_mod_0_pred_45, Draw_mod_0_pred_45, Away_mod_0_pred_45,
                Home_mod_0_pred_60, Draw_mod_0_pred_60, Away_mod_0_pred_60,
                Home_mod_0_pred_75, Draw_mod_0_pred_75, Away_mod_0_pred_75,
                
                Home_mod_B_pred_0, Draw_mod_B_pred_0, Away_mod_B_pred_0,
                Home_mod_B_pred_15, Draw_mod_B_pred_15, Away_mod_B_pred_15,
                Home_mod_B_pred_30, Draw_mod_B_pred_30, Away_mod_B_pred_30,
                Home_mod_B_pred_45, Draw_mod_B_pred_45, Away_mod_B_pred_45,
                Home_mod_B_pred_60, Draw_mod_B_pred_60, Away_mod_B_pred_60,
                Home_mod_B_pred_75, Draw_mod_B_pred_75, Away_mod_B_pred_75,
                
                Home_mod_C_pred_0, Draw_mod_C_pred_0, Away_mod_C_pred_0,
                Home_mod_C_pred_15, Draw_mod_C_pred_15, Away_mod_C_pred_15,
                Home_mod_C_pred_30, Draw_mod_C_pred_30, Away_mod_C_pred_30,
                Home_mod_C_pred_45, Draw_mod_C_pred_45, Away_mod_C_pred_45,
                Home_mod_C_pred_60, Draw_mod_C_pred_60, Away_mod_C_pred_60,
                Home_mod_C_pred_75, Draw_mod_C_pred_75, Away_mod_C_pred_75,
                
                Home_mod_3_pred_0, Draw_mod_3_pred_0, Away_mod_3_pred_0,
                Home_mod_3_pred_15, Draw_mod_3_pred_15, Away_mod_3_pred_15,
                Home_mod_3_pred_30, Draw_mod_3_pred_30, Away_mod_3_pred_30,
                Home_mod_3_pred_45, Draw_mod_3_pred_45, Away_mod_3_pred_45,
                Home_mod_3_pred_60, Draw_mod_3_pred_60, Away_mod_3_pred_60,
                Home_mod_3_pred_75, Draw_mod_3_pred_75, Away_mod_3_pred_75,
                
                Home_mod_12_pred_0, Draw_mod_12_pred_0, Away_mod_12_pred_0,
                Home_mod_12_pred_15, Draw_mod_12_pred_15, Away_mod_12_pred_15,
                Home_mod_12_pred_30, Draw_mod_12_pred_30, Away_mod_12_pred_30,
                Home_mod_12_pred_45, Draw_mod_12_pred_45, Away_mod_12_pred_45,
                Home_mod_12_pred_60, Draw_mod_12_pred_60, Away_mod_12_pred_60,
                Home_mod_12_pred_75, Draw_mod_12_pred_75, Away_mod_12_pred_75)
  
  hda = cbind(tmp1, tmp2)
  HDA_dc = rbind(HDA_dc, hda)
}

pnk_Result <- function(Result, pH, pD, pA) {
  p = c(pH, pD, pA)
  p[Result]
}

HDA_dc = HDA_dc %>%
  rowwise() %>%
  mutate(pnk_Result_mod_0_pred_0 = pnk_Result(Result, Home_mod_0_pred_0, Draw_mod_0_pred_0, Away_mod_0_pred_0),
         pnk_Result_mod_0_pred_15 = pnk_Result(Result, Home_mod_0_pred_15, Draw_mod_0_pred_15, Away_mod_0_pred_15),
         pnk_Result_mod_0_pred_30 = pnk_Result(Result, Home_mod_0_pred_30, Draw_mod_0_pred_30, Away_mod_0_pred_30),
         pnk_Result_mod_0_pred_45 = pnk_Result(Result, Home_mod_0_pred_45, Draw_mod_0_pred_45, Away_mod_0_pred_45),
         pnk_Result_mod_0_pred_60 = pnk_Result(Result, Home_mod_0_pred_60, Draw_mod_0_pred_60, Away_mod_0_pred_60),
         pnk_Result_mod_0_pred_75 = pnk_Result(Result, Home_mod_0_pred_75, Draw_mod_0_pred_75, Away_mod_0_pred_75),
         
         pnk_Result_mod_B_pred_0 = pnk_Result(Result, Home_mod_B_pred_0, Draw_mod_B_pred_0, Away_mod_B_pred_0),
         pnk_Result_mod_B_pred_15 = pnk_Result(Result, Home_mod_B_pred_15, Draw_mod_B_pred_15, Away_mod_B_pred_15),
         pnk_Result_mod_B_pred_30 = pnk_Result(Result, Home_mod_B_pred_30, Draw_mod_B_pred_30, Away_mod_B_pred_30),
         pnk_Result_mod_B_pred_45 = pnk_Result(Result, Home_mod_B_pred_45, Draw_mod_B_pred_45, Away_mod_B_pred_45),
         pnk_Result_mod_B_pred_60 = pnk_Result(Result, Home_mod_B_pred_60, Draw_mod_B_pred_60, Away_mod_B_pred_60),
         pnk_Result_mod_B_pred_75 = pnk_Result(Result, Home_mod_B_pred_75, Draw_mod_B_pred_75, Away_mod_B_pred_75),
         
         pnk_Result_mod_C_pred_0 = pnk_Result(Result, Home_mod_C_pred_0, Draw_mod_C_pred_0, Away_mod_C_pred_0),
         pnk_Result_mod_C_pred_15 = pnk_Result(Result, Home_mod_C_pred_15, Draw_mod_C_pred_15, Away_mod_C_pred_15),
         pnk_Result_mod_C_pred_30 = pnk_Result(Result, Home_mod_C_pred_30, Draw_mod_C_pred_30, Away_mod_C_pred_30),
         pnk_Result_mod_C_pred_45 = pnk_Result(Result, Home_mod_C_pred_45, Draw_mod_C_pred_45, Away_mod_C_pred_45),
         pnk_Result_mod_C_pred_60 = pnk_Result(Result, Home_mod_C_pred_60, Draw_mod_C_pred_60, Away_mod_C_pred_60),
         pnk_Result_mod_C_pred_75 = pnk_Result(Result, Home_mod_C_pred_75, Draw_mod_C_pred_75, Away_mod_C_pred_75),
         
         pnk_Result_mod_3_pred_0 = pnk_Result(Result, Home_mod_3_pred_0, Draw_mod_3_pred_0, Away_mod_3_pred_0),
         pnk_Result_mod_3_pred_15 = pnk_Result(Result, Home_mod_3_pred_15, Draw_mod_3_pred_15, Away_mod_3_pred_15),
         pnk_Result_mod_3_pred_30 = pnk_Result(Result, Home_mod_3_pred_30, Draw_mod_3_pred_30, Away_mod_3_pred_30),
         pnk_Result_mod_3_pred_45 = pnk_Result(Result, Home_mod_3_pred_45, Draw_mod_3_pred_45, Away_mod_3_pred_45),
         pnk_Result_mod_3_pred_60 = pnk_Result(Result, Home_mod_3_pred_60, Draw_mod_3_pred_60, Away_mod_3_pred_60),
         pnk_Result_mod_3_pred_75 = pnk_Result(Result, Home_mod_3_pred_75, Draw_mod_3_pred_75, Away_mod_3_pred_75),
         
         pnk_Result_mod_12_pred_0 = pnk_Result(Result, Home_mod_12_pred_0, Draw_mod_12_pred_0, Away_mod_12_pred_0),
         pnk_Result_mod_12_pred_15 = pnk_Result(Result, Home_mod_12_pred_15, Draw_mod_12_pred_15, Away_mod_12_pred_15),
         pnk_Result_mod_12_pred_30 = pnk_Result(Result, Home_mod_12_pred_30, Draw_mod_12_pred_30, Away_mod_12_pred_30),
         pnk_Result_mod_12_pred_45 = pnk_Result(Result, Home_mod_12_pred_45, Draw_mod_12_pred_45, Away_mod_12_pred_45),
         pnk_Result_mod_12_pred_60 = pnk_Result(Result, Home_mod_12_pred_60, Draw_mod_12_pred_60, Away_mod_12_pred_60),
         pnk_Result_mod_12_pred_75 = pnk_Result(Result, Home_mod_12_pred_75, Draw_mod_12_pred_75, Away_mod_12_pred_75))


pnk_Score <- function(Score, Match) {
  event = paste0(Match$Score_Home, "-", Match$Score_Away)
  p = Score[event]
  ifelse(is.na(p), 0, p)
}

pnk_Score_mod_0_pred_0 = NULL
pnk_Score_mod_0_pred_15 = NULL
pnk_Score_mod_0_pred_30 = NULL
pnk_Score_mod_0_pred_45 = NULL
pnk_Score_mod_0_pred_60 = NULL
pnk_Score_mod_0_pred_75 = NULL

pnk_Score_mod_B_pred_0 = NULL
pnk_Score_mod_B_pred_15 = NULL
pnk_Score_mod_B_pred_30 = NULL
pnk_Score_mod_B_pred_45 = NULL
pnk_Score_mod_B_pred_60 = NULL
pnk_Score_mod_B_pred_75 = NULL

pnk_Score_mod_C_pred_0 = NULL
pnk_Score_mod_C_pred_15 = NULL
pnk_Score_mod_C_pred_30 = NULL
pnk_Score_mod_C_pred_45 = NULL
pnk_Score_mod_C_pred_60 = NULL
pnk_Score_mod_C_pred_75 = NULL

pnk_Score_mod_3_pred_0 = NULL
pnk_Score_mod_3_pred_15 = NULL
pnk_Score_mod_3_pred_30 = NULL
pnk_Score_mod_3_pred_45 = NULL
pnk_Score_mod_3_pred_60 = NULL
pnk_Score_mod_3_pred_75 = NULL

pnk_Score_mod_12_pred_0 = NULL
pnk_Score_mod_12_pred_15 = NULL
pnk_Score_mod_12_pred_30 = NULL
pnk_Score_mod_12_pred_45 = NULL
pnk_Score_mod_12_pred_60 = NULL
pnk_Score_mod_12_pred_75 = NULL

for(i in 1:nrow(HDA_dc)) {
  pnk_Score_mod_0_pred_0[i] = pnk_Score(predictions_mod_0_dc[[i]]$pred_0$Score, predictions_mod_0_dc[[i]]$Match)
  pnk_Score_mod_0_pred_15[i] = pnk_Score(predictions_mod_0_dc[[i]]$pred_15$Score, predictions_mod_0_dc[[i]]$Match)
  pnk_Score_mod_0_pred_30[i] = pnk_Score(predictions_mod_0_dc[[i]]$pred_30$Score, predictions_mod_0_dc[[i]]$Match)
  pnk_Score_mod_0_pred_45[i] = pnk_Score(predictions_mod_0_dc[[i]]$pred_45$Score, predictions_mod_0_dc[[i]]$Match)
  pnk_Score_mod_0_pred_60[i] = pnk_Score(predictions_mod_0_dc[[i]]$pred_60$Score, predictions_mod_0_dc[[i]]$Match)
  pnk_Score_mod_0_pred_75[i] = pnk_Score(predictions_mod_0_dc[[i]]$pred_75$Score, predictions_mod_0_dc[[i]]$Match)
  
  pnk_Score_mod_B_pred_0[i] = pnk_Score(predictions_mod_B_dc[[i]]$pred_0$Score, predictions_mod_B_dc[[i]]$Match)
  pnk_Score_mod_B_pred_15[i] = pnk_Score(predictions_mod_B_dc[[i]]$pred_15$Score, predictions_mod_B_dc[[i]]$Match)
  pnk_Score_mod_B_pred_30[i] = pnk_Score(predictions_mod_B_dc[[i]]$pred_30$Score, predictions_mod_B_dc[[i]]$Match)
  pnk_Score_mod_B_pred_45[i] = pnk_Score(predictions_mod_B_dc[[i]]$pred_45$Score, predictions_mod_B_dc[[i]]$Match)
  pnk_Score_mod_B_pred_60[i] = pnk_Score(predictions_mod_B_dc[[i]]$pred_60$Score, predictions_mod_B_dc[[i]]$Match)
  pnk_Score_mod_B_pred_75[i] = pnk_Score(predictions_mod_B_dc[[i]]$pred_75$Score, predictions_mod_B_dc[[i]]$Match)
  
  pnk_Score_mod_C_pred_0[i] = pnk_Score(predictions_mod_C_dc[[i]]$pred_0$Score, predictions_mod_C_dc[[i]]$Match)
  pnk_Score_mod_C_pred_15[i] = pnk_Score(predictions_mod_C_dc[[i]]$pred_15$Score, predictions_mod_C_dc[[i]]$Match)
  pnk_Score_mod_C_pred_30[i] = pnk_Score(predictions_mod_C_dc[[i]]$pred_30$Score, predictions_mod_C_dc[[i]]$Match)
  pnk_Score_mod_C_pred_45[i] = pnk_Score(predictions_mod_C_dc[[i]]$pred_45$Score, predictions_mod_C_dc[[i]]$Match)
  pnk_Score_mod_C_pred_60[i] = pnk_Score(predictions_mod_C_dc[[i]]$pred_60$Score, predictions_mod_C_dc[[i]]$Match)
  pnk_Score_mod_C_pred_75[i] = pnk_Score(predictions_mod_C_dc[[i]]$pred_75$Score, predictions_mod_C_dc[[i]]$Match)
  
  pnk_Score_mod_3_pred_0[i] = pnk_Score(predictions_mod_3_dc[[i]]$pred_0$Score, predictions_mod_3_dc[[i]]$Match)
  pnk_Score_mod_3_pred_15[i] = pnk_Score(predictions_mod_3_dc[[i]]$pred_15$Score, predictions_mod_3_dc[[i]]$Match)
  pnk_Score_mod_3_pred_30[i] = pnk_Score(predictions_mod_3_dc[[i]]$pred_30$Score, predictions_mod_3_dc[[i]]$Match)
  pnk_Score_mod_3_pred_45[i] = pnk_Score(predictions_mod_3_dc[[i]]$pred_45$Score, predictions_mod_3_dc[[i]]$Match)
  pnk_Score_mod_3_pred_60[i] = pnk_Score(predictions_mod_3_dc[[i]]$pred_60$Score, predictions_mod_3_dc[[i]]$Match)
  pnk_Score_mod_3_pred_75[i] = pnk_Score(predictions_mod_3_dc[[i]]$pred_75$Score, predictions_mod_3_dc[[i]]$Match)
  
  pnk_Score_mod_12_pred_0[i] = pnk_Score(predictions_mod_12_dc[[i]]$pred_0$Score, predictions_mod_12_dc[[i]]$Match)
  pnk_Score_mod_12_pred_15[i] = pnk_Score(predictions_mod_12_dc[[i]]$pred_15$Score, predictions_mod_12_dc[[i]]$Match)
  pnk_Score_mod_12_pred_30[i] = pnk_Score(predictions_mod_12_dc[[i]]$pred_30$Score, predictions_mod_12_dc[[i]]$Match)
  pnk_Score_mod_12_pred_45[i] = pnk_Score(predictions_mod_12_dc[[i]]$pred_45$Score, predictions_mod_12_dc[[i]]$Match)
  pnk_Score_mod_12_pred_60[i] = pnk_Score(predictions_mod_12_dc[[i]]$pred_60$Score, predictions_mod_12_dc[[i]]$Match)
  pnk_Score_mod_12_pred_75[i] = pnk_Score(predictions_mod_12_dc[[i]]$pred_75$Score, predictions_mod_12_dc[[i]]$Match)
}

HDA_dc = cbind(HDA_dc, pnk_Score_mod_0_pred_0, pnk_Score_mod_0_pred_15, pnk_Score_mod_0_pred_30,
               pnk_Score_mod_0_pred_45, pnk_Score_mod_0_pred_60, pnk_Score_mod_0_pred_75,
               
               pnk_Score_mod_B_pred_0, pnk_Score_mod_B_pred_15, pnk_Score_mod_B_pred_30,
               pnk_Score_mod_B_pred_45, pnk_Score_mod_B_pred_60, pnk_Score_mod_B_pred_75,
               
               pnk_Score_mod_C_pred_0, pnk_Score_mod_C_pred_15, pnk_Score_mod_C_pred_30,
               pnk_Score_mod_C_pred_45, pnk_Score_mod_C_pred_60, pnk_Score_mod_C_pred_75, 
               
               pnk_Score_mod_3_pred_0, pnk_Score_mod_3_pred_15, pnk_Score_mod_3_pred_30,
               pnk_Score_mod_3_pred_45, pnk_Score_mod_3_pred_60, pnk_Score_mod_3_pred_75,
               
               pnk_Score_mod_12_pred_0, pnk_Score_mod_12_pred_15, pnk_Score_mod_12_pred_30,
               pnk_Score_mod_12_pred_45, pnk_Score_mod_12_pred_60, pnk_Score_mod_12_pred_75)

save(HDA_dc, file = "weight/data/HDA_dc_2_v2.RData")


