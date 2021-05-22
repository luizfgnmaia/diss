
library(dplyr)
library(stringr)

load("weight/data/predictions_mod_0_dc.RData")
load("weight/data/predictions_mod_3_dc.RData")
load("weight/data/predictions_mod_8_dc.RData")

load("weight/data/HDA_dc.RData")
load("weight/data/first_matches.RData")

first_matches = first_matches %>%
  mutate(tmp = 1)
HDA_dc = HDA_dc %>%
  left_join(first_matches)
matches_to_remove = which(HDA_dc$tmp == 1)

predictions_mod_0_dc = predictions_mod_0_dc[-matches_to_remove]
predictions_mod_3_dc = predictions_mod_3_dc[-matches_to_remove]
predictions_mod_8_dc = predictions_mod_8_dc[-matches_to_remove]

normal_pars <- function(pred) {
  
  mean_mod_0 = NULL
  var_mod_0 = NULL
  
  mean_mod_3 = NULL
  var_mod_3 = NULL
  
  mean_mod_8 = NULL
  var_mod_8 = NULL
  
  for(i in 1:length(predictions_mod_0_dc)) {
    
    predictions_mod_0_dc[[i]][[pred]]$Result[which(predictions_mod_0_dc[[i]][[pred]]$Result == 0)] = 10^-5
    pH = predictions_mod_0_dc[[i]][[pred]]$Result[1]
    pD = predictions_mod_0_dc[[i]][[pred]]$Result[2]
    pA = predictions_mod_0_dc[[i]][[pred]]$Result[3]
    mean_mod_0[i] = pH * log(pH) + pD * log(pD) + pA * log(pA)
    var_mod_0[i] = (pH * log(pH)^2 + pD * log(pD)^2 + pA * log(pA)^2) - mean_mod_0[i]^2
    
    predictions_mod_3_dc[[i]][[pred]]$Result[which(predictions_mod_3_dc[[i]][[pred]]$Result == 0)] = 10^-5
    pH = predictions_mod_3_dc[[i]][[pred]]$Result[1]
    pD = predictions_mod_3_dc[[i]][[pred]]$Result[2]
    pA = predictions_mod_3_dc[[i]][[pred]]$Result[3]
    mean_mod_3[i] = pH * log(pH) + pD * log(pD) + pA * log(pA)
    var_mod_3[i] = (pH * log(pH)^2 + pD * log(pD)^2 + pA * log(pA)^2) - mean_mod_3[i]^2
    
    predictions_mod_8_dc[[i]][[pred]]$Result[which(predictions_mod_8_dc[[i]][[pred]]$Result == 0)] = 10^-5
    pH = predictions_mod_8_dc[[i]][[pred]]$Result[1]
    pD = predictions_mod_8_dc[[i]][[pred]]$Result[2]
    pA = predictions_mod_8_dc[[i]][[pred]]$Result[3]
    mean_mod_8[i] = pH * log(pH) + pD * log(pD) + pA * log(pA)
    var_mod_8[i] = (pH * log(pH)^2 + pD * log(pD)^2 + pA * log(pA)^2) - mean_mod_8[i]^2
  }
  
  list(mean_mod_0 = sum(mean_mod_0),
       sd_mod_0 = sqrt(sum(var_mod_0)),
       
       mean_mod_3 = sum(mean_mod_3),
       sd_mod_3 = sqrt(sum(var_mod_3)),
       
       mean_mod_8 = sum(mean_mod_8),
       sd_mod_8 = sqrt(sum(var_mod_8)))
}

pars = list()
preds = paste0("pred_", c(0, 15, 30, 45, 60, 75))
for(i in 1:length(preds)) {
  pars[[i]] = normal_pars(preds[i])
}
names(pars) = preds

save(pars, file = "weight/data/pars.RData")