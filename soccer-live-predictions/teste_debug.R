
library(dplyr)
library(CVXR)


set.seed(1)

load("2020/data/input.RData")
load("2020/data/mod_0.RData")
load("2020/data/mod_3.RData")
source("pred/pred_mod_0.R")
source("pred/pred_mod_3.R")
source("pred/input_pred.R")

input_pred(ind = 363, min = 47)
input_pred(ind = 363, min = 48)
# input_pred tá funcionando certo

pred_mod_3(mod_3, 
           home_team = "Flamengo",
           away_team = "Internacional",
           score_home = 1,
           score_away = 1,
           minute = 2,
           half = 2)

pred_mod_3(mod_3, 
           home_team = "Flamengo",
           away_team = "Internacional",
           score_home = 1,
           score_away = 1,
           minute = 3,
           half = 2,
           reds_away_2 = 1)

# função também tá funcionando
# o problema era no próprio debug.R que não tava usando os cartões do segundo tempo
# mas o predictions para todos os jogos está certo
