
library(dplyr)

source("2020/pred_mod_1_dates.R")
load("2020/data/mod_1_dates.RData")
load("scrape/data/results.RData")

res = results %>%
  filter(Season == 2020) %>%
  arrange(Date)

tmp1 = res %>%
  filter(Date < "2020-08-15") %>%
  rename(Team = Home_Team) %>%
  count(Team, name = "n1")

tmp2 = res %>%
  filter(Date < "2020-08-15") %>%
  rename(Team = Away_Team) %>%
  count(Team, name = "n2")

games_per_team = full_join(tmp1, tmp2) %>%
  rowwise() %>%
  mutate(n = sum(n1, n2, na.rm = TRUE)) %>%
  select(-n1, -n2) %>%
  arrange(n)

games_per_team

mod_1_dates["2020-08-15"]

pred_mod_1(mod_1 = mod_1_dates[["2020-08-15"]], 
           home_team = "Palmeiras", 
           away_team = "Goiás")

pred_mod_1(mod_1 = mod_1_dates[["2020-08-15"]], 
           home_team = "Palmeiras", 
           away_team = "Goiás",
           stoppage_time = FALSE)

mod_1 = mod_1_dates[["2020-08-15"]]
reds_home = 0
reds_away = 0
reds_home_1 = 0
reds_away_1 = 0
rpois(1, lambda = mod_1$eta[2] + mod_1$rho[2]*(reds_home + reds_away - reds_home_1 - reds_away_1))
rpois(1, lambda = mod_1$eta[2] + mod_1$rho[2]*1)
rpois(1, lambda = mod_1$eta[2] + mod_1$rho[2]*2)
rpois(1, lambda = mod_1$eta[2] + mod_1$rho[2]*3)
mod_1$rho[2]
mod_1$eta[2] + mod_1$rho[2]*3
# O problema é que o mod_1$rho[2] é negativo e com muitas expulsões pode fazer com que o lambda da poisson fique negativo

