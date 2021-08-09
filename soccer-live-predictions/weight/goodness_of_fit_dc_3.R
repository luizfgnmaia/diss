
library(dplyr)
library(stringr)

load("weight/data/predictions_mod_0_dc.RData")
load("weight/data/predictions_mod_B_dc.RData")
load("weight/data/predictions_mod_C_dc.RData")
load("weight/data/predictions_mod_3_dc.RData")
load("weight/data/predictions_mod_12_dc.RData")

load("weight/data/HDA_dc_2.RData")
load("weight/data/first_matches.RData")

first_matches = first_matches %>%
  mutate(tmp = 1)
HDA_dc = HDA_dc %>%
  left_join(first_matches)
matches_to_remove = which(HDA_dc$tmp == 1)

predictions_mod_0_dc = predictions_mod_0_dc[-matches_to_remove]
predictions_mod_B_dc = predictions_mod_B_dc[-matches_to_remove]
predictions_mod_C_dc = predictions_mod_C_dc[-matches_to_remove]
predictions_mod_3_dc = predictions_mod_3_dc[-matches_to_remove]
predictions_mod_12_dc = predictions_mod_12_dc[-matches_to_remove]

probabilities <- function(predictions, pred) {
  
  H = NULL
  D = NULL
  A = NULL
  
  lst_home = list()
  lst_away = list()
  
  for(i in 1:length(predictions)) {
    
    H[i] = predictions[[i]][[pred]]$Result[1]
    D[i] = predictions[[i]][[pred]]$Result[2]
    A[i] = predictions[[i]][[pred]]$Result[3]
    
    score = predictions[[i]][[pred]]$Score
    home = str_extract(names(score), ".*(?=-)")
    away = str_extract(names(score), "(?<=-).*")
    tib_score = tibble()
    for(j in 1:length(score)) {
      tmp = tibble(Home = home[j], Away = away[j], p = score[j])
      tib_score = rbind(tib_score, tmp)
    }
    home_goals = NULL
    away_goals = NULL
    for(g in 0:4) {
      home_goals[g+1] = tib_score %>%
        filter(Home == g) %>%
        .$p %>%
        sum()
      away_goals[g+1] = tib_score %>%
        filter(Away == g) %>%
        .$p %>%
        sum()  
    }
    home_goals[6] = 1 - sum(home_goals)
    away_goals[6] = 1 - sum(away_goals)
    
    lst_home[[i]] = home_goals
    lst_away[[i]] = away_goals
  }
  
  
  
  Results = c(mean(H), mean(D), mean(A))
  names(Results) = c("Home", "Away", "Draw")
  
  mat_home = do.call(rbind, lst_home)
  mat_away = do.call(rbind, lst_away)
  Home_Goals = apply(mat_home, 2, mean)
  Away_Goals = apply(mat_away, 2, mean)
  names(Home_Goals) = c(0:4, "5+")
  names(Away_Goals) = c(0:4, "5+")
  
  list(Results = Results, Home_Goals = Home_Goals, Away_Goals = Away_Goals)
}

all_probabilities <- function(predictions) {
  ret = list()
  preds = paste0("pred_", c(0, 15, 30, 45, 60, 75))
  for(p in preds) {
    ret[[p]] = probabilities(predictions, p)
  }
  names(ret) = preds
  ret
}

prob_mod_0_dc = all_probabilities(predictions_mod_0_dc)
prob_mod_B_dc = all_probabilities(predictions_mod_B_dc)
prob_mod_C_dc = all_probabilities(predictions_mod_C_dc)
prob_mod_3_dc = all_probabilities(predictions_mod_3_dc)
prob_mod_12_dc = all_probabilities(predictions_mod_12_dc)

x = NULL
y = NULL
tmp = NULL
for(i in 1:length(predictions_mod_0_dc)) {
  x[i] = predictions_mod_0_dc[[i]]$Match$Score_Home
  y[i] = predictions_mod_0_dc[[i]]$Match$Score_Away
  tmp[i] = ifelse(x[i] > y[i], "Home",
                  ifelse(x[i] == y[i], "Draw",
                         "Away"))
}
Results = c(sum(tmp == "Home"), sum(tmp == "Draw"), sum(tmp == "Away")) / length(tmp)
Home_Goals = c(sum(x == 0), sum(x == 1), sum(x == 2), sum(x == 3), sum(x == 4), sum(x >= 5)) / length(x)
Away_Goals = c(sum(y == 0), sum(y == 1), sum(y == 2), sum(y == 3), sum(y == 4), sum(y >= 5)) / length(y)
tmp = list(Results = Results, Home_Goals = Home_Goals, Away_Goals = Away_Goals)

lst = list(tmp, prob_mod_0_dc$pred_0, prob_mod_B_dc$pred_0, prob_mod_C_dc$pred_0, prob_mod_3_dc$pred_0, prob_mod_12_dc$pred_0,
           prob_mod_0_dc$pred_15, prob_mod_B_dc$pred_15, prob_mod_C_dc$pred_15, prob_mod_3_dc$pred_15, prob_mod_12_dc$pred_15,
           prob_mod_0_dc$pred_30, prob_mod_B_dc$pred_30, prob_mod_C_dc$pred_30, prob_mod_3_dc$pred_30, prob_mod_12_dc$pred_30,
           prob_mod_0_dc$pred_45, prob_mod_B_dc$pred_45, prob_mod_C_dc$pred_45, prob_mod_3_dc$pred_45, prob_mod_12_dc$pred_45,
           prob_mod_0_dc$pred_60, prob_mod_B_dc$pred_60, prob_mod_C_dc$pred_60, prob_mod_3_dc$pred_60, prob_mod_12_dc$pred_60,
           prob_mod_0_dc$pred_75, prob_mod_B_dc$pred_75, prob_mod_C_dc$pred_75, prob_mod_3_dc$pred_75, prob_mod_12_dc$pred_75)

tab_results = matrix(NA, ncol = 3, nrow = 31)
tab_home_goals = matrix(NA, ncol = 6, nrow = 31)
tab_away_goals = matrix(NA, ncol = 6, nrow = 31)

for(i in 1:length(lst)) {
  tab_results[i,] = lst[[i]]$Results
  tab_home_goals[i,] = lst[[i]]$Home_Goals
  tab_away_goals[i,] = lst[[i]]$Away_Goals
}

rownames(tab_results) = c("Observed",
                          "Model 0 (min 0)",
                          "Model B (min 0)",
                          "Model C (min 0)",
                          "Model 3 (min 0)",
                          "Model 12 (min 0)",
                          "Model 0 (min 15)",
                          "Model B (min 15)",
                          "Model C (min 15)",
                          "Model 3 (min 15)",
                          "Model 12 (min 15)",
                          "Model 0 (min 30)",
                          "Model B (min 30)",
                          "Model C (min 30)",
                          "Model 3 (min 30)",
                          "Model 12 (min 30)",
                          "Model 0 (min 45)",
                          "Model B (min 45)",
                          "Model C (min 45)",
                          "Model 3 (min 45)",
                          "Model 12 (min 45)",
                          "Model 0 (min 60)",
                          "Model B (min 60)",
                          "Model C (min 60)",
                          "Model 3 (min 6)",
                          "Model 12 (min 60)",
                          "Model 0 (min 75)",
                          "Model B (min 75)",
                          "Model C (min 75)",
                          "Model 3 (min 75)",
                          "Model 12 (min 75)")

rownames(tab_home_goals) = rownames(tab_results)
rownames(tab_away_goals) = rownames(tab_results)
colnames(tab_results) = c("Home", "Draw", "Away")
colnames(tab_home_goals) = c(0:4, "5+")
colnames(tab_away_goals) = c(0:4, "5+")

save.image("weight/data/goodness_of_fit_dc_3.RData")

