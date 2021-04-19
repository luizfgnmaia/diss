
library(dplyr)
library(stringr)

load("pred/data/predictions_mod_0.RData")
load("pred/data/predictions_mod_3.RData")
load("pred/data/predictions_mod_3_2019.RData")
load("pred/data/predictions_mod_3_2015_2019.RData")
load("pred/data/HDA.RData")

set.seed(1)

ENERG1_mod_0_pred_0 = NULL
ENERG1_mod_0_pred_15 = NULL
ENERG1_mod_0_pred_30 = NULL
ENERG1_mod_0_pred_45 = NULL
ENERG1_mod_0_pred_60 = NULL
ENERG1_mod_0_pred_75 = NULL
ENERG1_mod_3_pred_0 = NULL
ENERG1_mod_3_pred_15 = NULL
ENERG1_mod_3_pred_30 = NULL
ENERG1_mod_3_pred_45 = NULL
ENERG1_mod_3_pred_60 = NULL
ENERG1_mod_3_pred_75 = NULL
ENERG1_mod_3_2019_pred_0 = NULL
ENERG1_mod_3_2019_pred_15 = NULL
ENERG1_mod_3_2019_pred_30 = NULL
ENERG1_mod_3_2019_pred_45 = NULL
ENERG1_mod_3_2019_pred_60 = NULL
ENERG1_mod_3_2019_pred_75 = NULL
ENERG1_mod_3_2015_2019_pred_0 = NULL
ENERG1_mod_3_2015_2019_pred_15 = NULL
ENERG1_mod_3_2015_2019_pred_30 = NULL
ENERG1_mod_3_2015_2019_pred_45 = NULL
ENERG1_mod_3_2015_2019_pred_60 = NULL
ENERG1_mod_3_2015_2019_pred_75 = NULL

ENERG1 <- function(Score, Match, size = 10^6) {
  x1 = Match$Score_Home
  x2 = Match$Score_Away
  
  X = sample(names(Score), size = size, replace = TRUE, prob = Score)
  X1 = as.integer(str_extract(X, "[0-9]*(?=-)"))
  X2 = as.integer(str_extract(X, "(?<=-)[0-9]*"))
  EX = mean(abs(X1 - x1) + abs(X2 - x2))
  
  X = sample(names(Score), size = size, replace = TRUE, prob = Score)
  X1 = as.integer(str_extract(X, "[0-9]*(?=-)"))
  X2 = as.integer(str_extract(X, "(?<=-)[0-9]*"))
  XL = sample(names(Score), size = size, replace = TRUE, prob = Score)
  XL1 = as.integer(str_extract(XL, "[0-9]*(?=-)"))
  XL2 = as.integer(str_extract(XL, "(?<=-)[0-9]*"))
  
  EXXL = mean(abs(X1 - XL1) + abs(X2 - XL2))
  
  EX - 0.5*EXXL
}

for(i in 1:nrow(HDA)) {
  ENERG1_mod_0_pred_0[i] = ENERG1(predictions_mod_0[[i]]$pred_0$Score, predictions_mod_0[[i]]$Match)
  ENERG1_mod_0_pred_15[i] = ENERG1(predictions_mod_0[[i]]$pred_15$Score, predictions_mod_0[[i]]$Match)
  ENERG1_mod_0_pred_30[i] = ENERG1(predictions_mod_0[[i]]$pred_30$Score, predictions_mod_0[[i]]$Match)
  ENERG1_mod_0_pred_45[i] = ENERG1(predictions_mod_0[[i]]$pred_45$Score, predictions_mod_0[[i]]$Match)
  ENERG1_mod_0_pred_60[i] = ENERG1(predictions_mod_0[[i]]$pred_60$Score, predictions_mod_0[[i]]$Match)
  ENERG1_mod_0_pred_75[i] = ENERG1(predictions_mod_0[[i]]$pred_75$Score, predictions_mod_0[[i]]$Match)
  
  ENERG1_mod_3_pred_0[i] = ENERG1(predictions_mod_3[[i]]$pred_0$Score, predictions_mod_3[[i]]$Match)
  ENERG1_mod_3_pred_15[i] = ENERG1(predictions_mod_3[[i]]$pred_15$Score, predictions_mod_3[[i]]$Match)
  ENERG1_mod_3_pred_30[i] = ENERG1(predictions_mod_3[[i]]$pred_30$Score, predictions_mod_3[[i]]$Match)
  ENERG1_mod_3_pred_45[i] = ENERG1(predictions_mod_3[[i]]$pred_45$Score, predictions_mod_3[[i]]$Match)
  ENERG1_mod_3_pred_60[i] = ENERG1(predictions_mod_3[[i]]$pred_60$Score, predictions_mod_3[[i]]$Match)
  ENERG1_mod_3_pred_75[i] = ENERG1(predictions_mod_3[[i]]$pred_75$Score, predictions_mod_3[[i]]$Match)
  
  ENERG1_mod_3_2019_pred_0[i] = ENERG1(predictions_mod_3_2019[[i]]$pred_0$Score, predictions_mod_3_2019[[i]]$Match)
  ENERG1_mod_3_2019_pred_15[i] = ENERG1(predictions_mod_3_2019[[i]]$pred_15$Score, predictions_mod_3_2019[[i]]$Match)
  ENERG1_mod_3_2019_pred_30[i] = ENERG1(predictions_mod_3_2019[[i]]$pred_30$Score, predictions_mod_3_2019[[i]]$Match)
  ENERG1_mod_3_2019_pred_45[i] = ENERG1(predictions_mod_3_2019[[i]]$pred_45$Score, predictions_mod_3_2019[[i]]$Match)
  ENERG1_mod_3_2019_pred_60[i] = ENERG1(predictions_mod_3_2019[[i]]$pred_60$Score, predictions_mod_3_2019[[i]]$Match)
  ENERG1_mod_3_2019_pred_75[i] = ENERG1(predictions_mod_3_2019[[i]]$pred_75$Score, predictions_mod_3_2019[[i]]$Match)
  
  ENERG1_mod_3_2015_2019_pred_0[i] = ENERG1(predictions_mod_3_2015_2019[[i]]$pred_0$Score, predictions_mod_3_2015_2019[[i]]$Match)
  ENERG1_mod_3_2015_2019_pred_15[i] = ENERG1(predictions_mod_3_2015_2019[[i]]$pred_15$Score, predictions_mod_3_2015_2019[[i]]$Match)
  ENERG1_mod_3_2015_2019_pred_30[i] = ENERG1(predictions_mod_3_2015_2019[[i]]$pred_30$Score, predictions_mod_3_2015_2019[[i]]$Match)
  ENERG1_mod_3_2015_2019_pred_45[i] = ENERG1(predictions_mod_3_2015_2019[[i]]$pred_45$Score, predictions_mod_3_2015_2019[[i]]$Match)
  ENERG1_mod_3_2015_2019_pred_60[i] = ENERG1(predictions_mod_3_2015_2019[[i]]$pred_60$Score, predictions_mod_3_2015_2019[[i]]$Match)
  ENERG1_mod_3_2015_2019_pred_75[i] = ENERG1(predictions_mod_3_2015_2019[[i]]$pred_75$Score, predictions_mod_3_2015_2019[[i]]$Match)
  
  print(paste0(round(100*i/nrow(HDA), 2), "%"))
}

HDA2 = cbind(HDA, ENERG1_mod_0_pred_0, ENERG1_mod_0_pred_15, ENERG1_mod_0_pred_30, 
             ENERG1_mod_0_pred_45, ENERG1_mod_0_pred_60, ENERG1_mod_0_pred_75,
             ENERG1_mod_3_pred_0, ENERG1_mod_3_pred_15, ENERG1_mod_3_pred_30, 
             ENERG1_mod_3_pred_45, ENERG1_mod_3_pred_60, ENERG1_mod_3_pred_75,
             ENERG1_mod_3_2019_pred_0, ENERG1_mod_3_2019_pred_15, ENERG1_mod_3_2019_pred_30, 
             ENERG1_mod_3_2019_pred_45, ENERG1_mod_3_2019_pred_60, ENERG1_mod_3_2019_pred_75,
             ENERG1_mod_3_2015_2019_pred_0, ENERG1_mod_3_2015_2019_pred_15, ENERG1_mod_3_2015_2019_pred_30, 
             ENERG1_mod_3_2015_2019_pred_45, ENERG1_mod_3_2015_2019_pred_60, ENERG1_mod_3_2015_2019_pred_75)

save(HDA2, file = "pred/data/HDA2.RData")


