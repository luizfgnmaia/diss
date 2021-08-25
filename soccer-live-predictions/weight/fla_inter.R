
set.seed(1)

options(OutDec = ",")

library(dplyr)
library(tidyr)
library(ggplot2)
library(future.apply)
plan(multisession(workers = 12)) 

source("pred/input_pred.R")
source("pred/input_value.R")
source("pred/pred_mod_12.R")
load("transfermarkt/data/results.RData")
load("scrape/data/goals.RData")
load("scrape/data/reds.RData")
load("weight/data/mod_12_dc.RData")
load("2015-2020/data/input.RData")

n_pred = 3*10^5 # <--- tava 10^5
mod = mod_12_dc$`2021-02-21`
ind = 2262
pred = list()
values = input_value("Flamengo", "Internacional", "2021-02-21")

for(m in 0:44) {
  input = input_pred(ind = 2262, min = m)
  pred[[m+1]] = pred_mod_12(mod_12 = mod, 
                            n = n_pred, 
                            home_team = "Flamengo", 
                            away_team = "Internacional",
                            score_home = input$score_home,
                            score_away = input$score_away,
                            reds_home_1 = input$reds_home_1,
                            reds_away_1 = input$reds_away_1,
                            minute = m,
                            half = 1,
                            value_home = values$Value_Home,
                            value_away = values$Value_Away)
}

for(m in 45:90) {
  input = input_pred(ind = 2262, min = m)
  pred[[m+1]] = pred_mod_12(mod_12 = mod, 
                            n = n_pred, 
                            home_team = "Flamengo", 
                            away_team = "Internacional",
                            score_home = input$score_home,
                            score_away = input$score_away,
                            reds_home_1 = input$reds_home_1,
                            reds_away_1 = input$reds_away_1,
                            reds_home_2 = input$reds_home_2,
                            reds_away_2 = input$reds_away_2,
                            minute = m-45,
                            half = 2,
                            value_home = values$Value_Home,
                            value_away = values$Value_Away)
}

save(pred, file = "weight/data/pred_fla_inter.RData")

#################################################################################
#################################################################################
#################################################################################

# load("weight/data/pred_fla_inter.RData")

wide = tibble()
for(i in 1:91) {
  wide = rbind(wide, pred[[i]]$Result)
}
names(wide) = c("Flamengo", "Empate", "Internacional")
wide$Minuto = 0:90

long = wide %>%
  pivot_longer(cols = c("Flamengo", "Empate", "Internacional"),
               names_to = "Resultado",
               values_to = "Probabilidade")

p = long %>%
  mutate(Resultado = factor(Resultado, levels = c("Flamengo", "Empate", "Internacional"))) %>%
  ggplot(aes(x = Minuto, y = Probabilidade, col = Resultado)) +
  geom_vline(xintercept = 11, linetype = "dashed") +
  geom_vline(xintercept = 28, linetype = "dashed") +
  geom_vline(xintercept = 17+45, linetype = "dashed") +
  geom_vline(xintercept = 3+45, linetype = "dashed", col = "red") +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90))
p

ggsave(filename = paste0("weight/plots/fla_inter.png"),
       plot = p, width = 10, height = 5, dpi = 1000)



