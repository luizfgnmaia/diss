
load("weight/data/mod_0_dc.RData")
load("weight/data/mod_12_dc.RData")
load("transfermarkt/data/results.RData")

mod_0 = mod_0_dc$`2021-02-25`
mod_12 = mod_12_dc$`2021-02-25`
dif_value = 0.33764251
st = 6+8
st = 0

# Internacional e Corinthians

rate_home_mod_0 = exp(mod_0$alpha["Internacional"] + mod_0$beta["Corinthians"] + mod_0$gamma) / (90+st)
rate_away_mod_0 = exp(mod_0$alpha["Corinthians"] + mod_0$beta["Internacional"]) / (90+st)

rate_home_mod_12_1 = exp(mod_12$alpha["Internacional"] + mod_12$beta["Corinthians"] + mod_12$gamma + mod_12$omega["value"]*(dif_value))
rate_home_mod_12_2 = exp(mod_12$tau + mod_12$alpha["Internacional"] + mod_12$beta["Corinthians"] + mod_12$gamma + mod_12$omega["value"]*(dif_value))
rate_away_mod_12_1 = exp(mod_12$alpha["Corinthians"] + mod_12$beta["Internacional"] + mod_12$omega["value"]*(dif_value))
rate_away_mod_12_2 = exp(mod_12$tau + mod_12$alpha["Corinthians"] + mod_12$beta["Internacional"] + mod_12$omega["value"]*(dif_value))

home = c(rate_home_mod_0, rate_home_mod_12_1, rate_home_mod_12_2)
away = c(rate_away_mod_0, rate_away_mod_12_1, rate_away_mod_12_2)
rate = c("Model 0", "Model 12 1st half", "Model 12 2nd half")

(df = data.frame(rate, home, away))





