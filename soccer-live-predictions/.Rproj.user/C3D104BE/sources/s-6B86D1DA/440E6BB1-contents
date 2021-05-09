# deletar o outro depois e renomear esse pra _1
# predictions = predictions_mod_0_dc
# pred = "pred_15"


library(dplyr)
library(stringr)

load("weight/data/predictions_mod_0_dc.RData")
load("weight/data/predictions_mod_3_dc.RData")
load("weight/data/predictions_mod_8_dc.RData")

load("weight/data/HDA_dc.RData")
load("weight/data/first_matches.RData")

set.seed(1)

first_matches = first_matches %>%
  mutate(tmp = 1)
HDA_dc = HDA_dc %>%
  left_join(first_matches)
matches_to_remove = which(HDA_dc$tmp == 1)

predictions_mod_0_dc = predictions_mod_0_dc[-matches_to_remove]
predictions_mod_3_dc = predictions_mod_3_dc[-matches_to_remove]
predictions_mod_8_dc = predictions_mod_8_dc[-matches_to_remove]

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
prob_mod_3_dc = all_probabilities(predictions_mod_3_dc)
prob_mod_8_dc = all_probabilities(predictions_mod_8_dc)

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

lst = list(tmp, prob_mod_0_dc$pred_0, prob_mod_3_dc$pred_0, prob_mod_8_dc$pred_0,
           prob_mod_0_dc$pred_15, prob_mod_3_dc$pred_15, prob_mod_8_dc$pred_15,
           prob_mod_0_dc$pred_30, prob_mod_3_dc$pred_30, prob_mod_8_dc$pred_30,
           prob_mod_0_dc$pred_45, prob_mod_3_dc$pred_45, prob_mod_8_dc$pred_45,
           prob_mod_0_dc$pred_60, prob_mod_3_dc$pred_60, prob_mod_8_dc$pred_60,
           prob_mod_0_dc$pred_75, prob_mod_3_dc$pred_75, prob_mod_8_dc$pred_75)

tab_results = matrix(NA, ncol = 3, nrow = 19)
tab_home_goals = matrix(NA, ncol = 6, nrow = 19)
tab_away_goals = matrix(NA, ncol = 6, nrow = 19)

for(i in 1:length(lst)) {
  tab_results[i,] = lst[[i]]$Results
  tab_home_goals[i,] = lst[[i]]$Home_Goals
  tab_away_goals[i,] = lst[[i]]$Away_Goals
}

rownames(tab_results) = c("Observed",
                          "Model 0 (min 0)",
                          "Model 3 (min 0)",
                          "Model 8 (min 0)",
                          "Model 0 (min 15)",
                          "Model 3 (min 15)",
                          "Model 8 (min 15)",
                          "Model 0 (min 30)",
                          "Model 3 (min 30)",
                          "Model 8 (min 30)",
                          "Model 0 (min 45)",
                          "Model 3 (min 45)",
                          "Model 8 (min 45)",
                          "Model 0 (min 60)",
                          "Model 3 (min 60)",
                          "Model 8 (min 60)",
                          "Model 0 (min 75)",
                          "Model 3 (min 75)",
                          "Model 8 (min 75)")

rownames(tab_home_goals) = rownames(tab_results)
rownames(tab_away_goals) = rownames(tab_results)
colnames(tab_results) = c("Home", "Draw", "Away")
colnames(tab_home_goals) = c(0:4, "5+")
colnames(tab_away_goals) = c(0:4, "5+")

#################################################################################
#################################################################################
#################################################################################

pnk_Result <- function(Result, Match) {
  res = ifelse(Match$Score_Home > Match$Score_Away, 1,
               ifelse(Match$Score_Home == Match$Score_Away, 2,
                      3))
  Result[res]
}

pnk_Score <- function(Score, Match) {
  event = paste0(Match$Score_Home, "-", Match$Score_Away)
  p = Score[event]
  ifelse(is.na(p), 10^-5, p) # para o produtório não virar 0
}

loglik_observed_results_mod_0_pred_0 = NULL
loglik_observed_scores_mod_0_pred_0 = NULL
loglik_observed_results_mod_3_pred_0 = NULL
loglik_observed_scores_mod_3_pred_0 = NULL
loglik_observed_results_mod_8_pred_0 = NULL
loglik_observed_scores_mod_8_pred_0 = NULL

loglik_observed_results_mod_0_pred_15 = NULL
loglik_observed_scores_mod_0_pred_15 = NULL
loglik_observed_results_mod_3_pred_15 = NULL
loglik_observed_scores_mod_3_pred_15 = NULL
loglik_observed_results_mod_8_pred_15 = NULL
loglik_observed_scores_mod_8_pred_15 = NULL

loglik_observed_results_mod_0_pred_30 = NULL
loglik_observed_scores_mod_0_pred_30 = NULL
loglik_observed_results_mod_3_pred_30 = NULL
loglik_observed_scores_mod_3_pred_30 = NULL
loglik_observed_results_mod_8_pred_30 = NULL
loglik_observed_scores_mod_8_pred_30 = NULL

loglik_observed_results_mod_0_pred_45 = NULL
loglik_observed_scores_mod_0_pred_45 = NULL
loglik_observed_results_mod_3_pred_45 = NULL
loglik_observed_scores_mod_3_pred_45 = NULL
loglik_observed_results_mod_8_pred_45 = NULL
loglik_observed_scores_mod_8_pred_45 = NULL

loglik_observed_results_mod_0_pred_60 = NULL
loglik_observed_scores_mod_0_pred_60 = NULL
loglik_observed_results_mod_3_pred_60 = NULL
loglik_observed_scores_mod_3_pred_60 = NULL
loglik_observed_results_mod_8_pred_60 = NULL
loglik_observed_scores_mod_8_pred_60 = NULL

loglik_observed_results_mod_0_pred_75 = NULL
loglik_observed_scores_mod_0_pred_75 = NULL
loglik_observed_results_mod_3_pred_75 = NULL
loglik_observed_scores_mod_3_pred_75 = NULL
loglik_observed_results_mod_8_pred_75 = NULL
loglik_observed_scores_mod_8_pred_75 = NULL

for(i in 1:length(predictions_mod_0_dc)) {
  loglik_observed_results_mod_0_pred_0[i] = log(pnk_Result(predictions_mod_0_dc[[i]]$pred_0$Result, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_scores_mod_0_pred_0[i] = log(pnk_Score(predictions_mod_0_dc[[i]]$pred_0$Score, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_results_mod_3_pred_0[i] = log(pnk_Result(predictions_mod_3_dc[[i]]$pred_0$Result, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_scores_mod_3_pred_0[i] = log(pnk_Score(predictions_mod_3_dc[[i]]$pred_0$Score, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_results_mod_8_pred_0[i] = log(pnk_Result(predictions_mod_8_dc[[i]]$pred_0$Result, predictions_mod_8_dc[[i]]$Match))
  loglik_observed_scores_mod_8_pred_0[i] = log(pnk_Score(predictions_mod_8_dc[[i]]$pred_0$Score, predictions_mod_8_dc[[i]]$Match))
  
  loglik_observed_results_mod_0_pred_15[i] = log(pnk_Result(predictions_mod_0_dc[[i]]$pred_15$Result, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_scores_mod_0_pred_15[i] = log(pnk_Score(predictions_mod_0_dc[[i]]$pred_15$Score, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_results_mod_3_pred_15[i] = log(pnk_Result(predictions_mod_3_dc[[i]]$pred_15$Result, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_scores_mod_3_pred_15[i] = log(pnk_Score(predictions_mod_3_dc[[i]]$pred_15$Score, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_results_mod_8_pred_15[i] = log(pnk_Result(predictions_mod_8_dc[[i]]$pred_15$Result, predictions_mod_8_dc[[i]]$Match))
  loglik_observed_scores_mod_8_pred_15[i] = log(pnk_Score(predictions_mod_8_dc[[i]]$pred_15$Score, predictions_mod_8_dc[[i]]$Match))
  
  loglik_observed_results_mod_0_pred_30[i] = log(pnk_Result(predictions_mod_0_dc[[i]]$pred_30$Result, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_scores_mod_0_pred_30[i] = log(pnk_Score(predictions_mod_0_dc[[i]]$pred_30$Score, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_results_mod_3_pred_30[i] = log(pnk_Result(predictions_mod_3_dc[[i]]$pred_30$Result, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_scores_mod_3_pred_30[i] = log(pnk_Score(predictions_mod_3_dc[[i]]$pred_30$Score, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_results_mod_8_pred_30[i] = log(pnk_Result(predictions_mod_8_dc[[i]]$pred_30$Result, predictions_mod_8_dc[[i]]$Match))
  loglik_observed_scores_mod_8_pred_30[i] = log(pnk_Score(predictions_mod_8_dc[[i]]$pred_30$Score, predictions_mod_8_dc[[i]]$Match))
  
  loglik_observed_results_mod_0_pred_45[i] = log(pnk_Result(predictions_mod_0_dc[[i]]$pred_45$Result, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_scores_mod_0_pred_45[i] = log(pnk_Score(predictions_mod_0_dc[[i]]$pred_45$Score, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_results_mod_3_pred_45[i] = log(pnk_Result(predictions_mod_3_dc[[i]]$pred_45$Result, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_scores_mod_3_pred_45[i] = log(pnk_Score(predictions_mod_3_dc[[i]]$pred_45$Score, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_results_mod_8_pred_45[i] = log(pnk_Result(predictions_mod_8_dc[[i]]$pred_45$Result, predictions_mod_8_dc[[i]]$Match))
  loglik_observed_scores_mod_8_pred_45[i] = log(pnk_Score(predictions_mod_8_dc[[i]]$pred_45$Score, predictions_mod_8_dc[[i]]$Match))
  
  loglik_observed_results_mod_0_pred_60[i] = log(pnk_Result(predictions_mod_0_dc[[i]]$pred_60$Result, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_scores_mod_0_pred_60[i] = log(pnk_Score(predictions_mod_0_dc[[i]]$pred_60$Score, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_results_mod_3_pred_60[i] = log(pnk_Result(predictions_mod_3_dc[[i]]$pred_60$Result, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_scores_mod_3_pred_60[i] = log(pnk_Score(predictions_mod_3_dc[[i]]$pred_60$Score, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_results_mod_8_pred_60[i] = log(pnk_Result(predictions_mod_8_dc[[i]]$pred_60$Result, predictions_mod_8_dc[[i]]$Match))
  loglik_observed_scores_mod_8_pred_60[i] = log(pnk_Score(predictions_mod_8_dc[[i]]$pred_60$Score, predictions_mod_8_dc[[i]]$Match))
  
  loglik_observed_results_mod_0_pred_75[i] = log(pnk_Result(predictions_mod_0_dc[[i]]$pred_75$Result, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_scores_mod_0_pred_75[i] = log(pnk_Score(predictions_mod_0_dc[[i]]$pred_75$Score, predictions_mod_0_dc[[i]]$Match))
  loglik_observed_results_mod_3_pred_75[i] = log(pnk_Result(predictions_mod_3_dc[[i]]$pred_75$Result, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_scores_mod_3_pred_75[i] = log(pnk_Score(predictions_mod_3_dc[[i]]$pred_75$Score, predictions_mod_3_dc[[i]]$Match))
  loglik_observed_results_mod_8_pred_75[i] = log(pnk_Result(predictions_mod_8_dc[[i]]$pred_75$Result, predictions_mod_8_dc[[i]]$Match))
  loglik_observed_scores_mod_8_pred_75[i] = log(pnk_Score(predictions_mod_8_dc[[i]]$pred_75$Score, predictions_mod_8_dc[[i]]$Match))
}

loglik_observed_results_mod_0_pred_0 = sum(loglik_observed_results_mod_0_pred_0)
loglik_observed_scores_mod_0_pred_0 = sum(loglik_observed_scores_mod_0_pred_0)
loglik_observed_results_mod_3_pred_0 = sum(loglik_observed_results_mod_3_pred_0)
loglik_observed_scores_mod_3_pred_0 = sum(loglik_observed_scores_mod_3_pred_0)
loglik_observed_results_mod_8_pred_0 = sum(loglik_observed_results_mod_8_pred_0)
loglik_observed_scores_mod_8_pred_0 = sum(loglik_observed_scores_mod_8_pred_0)

loglik_observed_results_mod_0_pred_15 = sum(loglik_observed_results_mod_0_pred_15)
loglik_observed_scores_mod_0_pred_15 = sum(loglik_observed_scores_mod_0_pred_15)
loglik_observed_results_mod_3_pred_15 = sum(loglik_observed_results_mod_3_pred_15)
loglik_observed_scores_mod_3_pred_15 = sum(loglik_observed_scores_mod_3_pred_15)
loglik_observed_results_mod_8_pred_15 = sum(loglik_observed_results_mod_8_pred_15)
loglik_observed_scores_mod_8_pred_15 = sum(loglik_observed_scores_mod_8_pred_15)

loglik_observed_results_mod_0_pred_30 = sum(loglik_observed_results_mod_0_pred_30)
loglik_observed_scores_mod_0_pred_30 = sum(loglik_observed_scores_mod_0_pred_30)
loglik_observed_results_mod_3_pred_30 = sum(loglik_observed_results_mod_3_pred_30)
loglik_observed_scores_mod_3_pred_30 = sum(loglik_observed_scores_mod_3_pred_30)
loglik_observed_results_mod_8_pred_30 = sum(loglik_observed_results_mod_8_pred_30)
loglik_observed_scores_mod_8_pred_30 = sum(loglik_observed_scores_mod_8_pred_30)

loglik_observed_results_mod_0_pred_45 = sum(loglik_observed_results_mod_0_pred_45)
loglik_observed_scores_mod_0_pred_45 = sum(loglik_observed_scores_mod_0_pred_45)
loglik_observed_results_mod_3_pred_45 = sum(loglik_observed_results_mod_3_pred_45)
loglik_observed_scores_mod_3_pred_45 = sum(loglik_observed_scores_mod_3_pred_45)
loglik_observed_results_mod_8_pred_45 = sum(loglik_observed_results_mod_8_pred_45)
loglik_observed_scores_mod_8_pred_45 = sum(loglik_observed_scores_mod_8_pred_45)

loglik_observed_results_mod_0_pred_60 = sum(loglik_observed_results_mod_0_pred_60)
loglik_observed_scores_mod_0_pred_60 = sum(loglik_observed_scores_mod_0_pred_60)
loglik_observed_results_mod_3_pred_60 = sum(loglik_observed_results_mod_3_pred_60)
loglik_observed_scores_mod_3_pred_60 = sum(loglik_observed_scores_mod_3_pred_60)
loglik_observed_results_mod_8_pred_60 = sum(loglik_observed_results_mod_8_pred_60)
loglik_observed_scores_mod_8_pred_60 = sum(loglik_observed_scores_mod_8_pred_60)

loglik_observed_results_mod_0_pred_75 = sum(loglik_observed_results_mod_0_pred_75)
loglik_observed_scores_mod_0_pred_75 = sum(loglik_observed_scores_mod_0_pred_75)
loglik_observed_results_mod_3_pred_75 = sum(loglik_observed_results_mod_3_pred_75)
loglik_observed_scores_mod_3_pred_75 = sum(loglik_observed_scores_mod_3_pred_75)
loglik_observed_results_mod_8_pred_75 = sum(loglik_observed_results_mod_8_pred_75)
loglik_observed_scores_mod_8_pred_75 = sum(loglik_observed_scores_mod_8_pred_75)

sim_all_matches <- function(pred) {
  
  loglik_results_mod_0 = NULL
  loglik_scores_mod_0 = NULL
  loglik_results_mod_3 = NULL
  loglik_scores_mod_3 = NULL
  loglik_results_mod_8 = NULL
  loglik_scores_mod_8 = NULL
  
  for(i in 1:length(predictions_mod_0_dc)) {
    loglik_results_mod_0[i] = log(sample(predictions_mod_0_dc[[i]][[pred]]$Result, size = 1, prob = predictions_mod_0_dc[[i]][[pred]]$Result))
    loglik_scores_mod_0[i] = log(sample(predictions_mod_0_dc[[i]][[pred]]$Score, size = 1, prob = predictions_mod_0_dc[[i]][[pred]]$Score))
    loglik_results_mod_3[i] = log(sample(predictions_mod_3_dc[[i]][[pred]]$Result, size = 1, prob = predictions_mod_3_dc[[i]][[pred]]$Result))
    loglik_scores_mod_3[i] = log(sample(predictions_mod_3_dc[[i]][[pred]]$Score, size = 1, prob = predictions_mod_3_dc[[i]][[pred]]$Score))
    loglik_results_mod_8[i] = log(sample(predictions_mod_8_dc[[i]][[pred]]$Result, size = 1, prob = predictions_mod_8_dc[[i]][[pred]]$Result))
    loglik_scores_mod_8[i] = log(sample(predictions_mod_8_dc[[i]][[pred]]$Score, size = 1, prob = predictions_mod_8_dc[[i]][[pred]]$Score))
  }
  
  list(loglik_results_mod_0 = sum(loglik_results_mod_0),
       loglik_scores_mod_0 = sum(loglik_scores_mod_0),
       loglik_results_mod_3 = sum(loglik_results_mod_3),
       loglik_scores_mod_3 = sum(loglik_scores_mod_3),
       loglik_results_mod_8 = sum(loglik_results_mod_8),
       loglik_scores_mod_8 = sum(loglik_scores_mod_8))
}

sim_all_matches_n <- function(n, pred) {
  ret_results_mod_0 = NULL
  ret_scores_mod_0 = NULL
  ret_results_mod_3 = NULL
  ret_scores_mod_3 = NULL
  ret_results_mod_8 = NULL
  ret_scores_mod_8 = NULL
  for(i in 1:n) {
    sim = sim_all_matches(pred)
    ret_results_mod_0[i] = sim$loglik_results_mod_0
    ret_scores_mod_0[i] = sim$loglik_scores_mod_0
    ret_results_mod_3[i] = sim$loglik_results_mod_3
    ret_scores_mod_3[i] = sim$loglik_scores_mod_3
    ret_results_mod_8[i] = sim$loglik_results_mod_8
    ret_scores_mod_8[i] = sim$loglik_scores_mod_8
    print(paste0(round(100*i/n, 2), "% ", pred))
  }
  list(loglik_results_mod_0 = ret_results_mod_0,
       loglik_scores_mod_0 = ret_scores_mod_0,
       loglik_results_mod_3 = ret_results_mod_3,
       loglik_scores_mod_3 = ret_scores_mod_3,
       loglik_results_mod_8 = ret_results_mod_8,
       loglik_scores_mod_8 = ret_scores_mod_8)
}

sims = list()
preds = paste0("pred_", c(0, 15, 30, 45, 60, 75))
for(i in 1:length(preds)) {
  sims[[i]] = sim_all_matches_n(10^5, preds[i])
}
names(sims) = preds

save.image("weight/data/goodness_of_fit_dc.RData")

