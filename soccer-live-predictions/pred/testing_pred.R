
library(dplyr)
library(future.apply)
plan(multisession(workers = 12)) 

source("pred/pred_mod_8.R")
source("pred/pred_mod_9.R")
source("pred/pred_mod_10.R")
load("2015-2020/data/mod_8.RData")
load("2015-2020/data/mod_9.RData")
load("2015-2020/data/mod_10.RData")
load("transfermarkt/data/results.RData")

pred_mod_8(mod_8, home_team = "Flamengo", away_team = "Vasco da Gama")

pred_mod_9(mod_9, home_team = "Flamengo", away_team = "Vasco da Gama", value_home = 79050000, value_away = 23150000)

pred_mod_10(mod_10, home_team = "Flamengo", away_team = "Vasco da Gama", value_home = 79050000, value_away = 23150000)

pred_mod_9(mod_9, home_team = "Flamengo", away_team = "Vasco da Gama", value_home = 790500000, value_away = 23150000)

pred_mod_10(mod_10, home_team = "Flamengo", away_team = "Vasco da Gama", value_home = 790500000, value_away = 23150000)
