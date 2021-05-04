
library(dplyr)

load("weight/data/goodness_of_fit_dc.RData")
load("weight/data/predictions_mod_0_dc.RData")
load("weight/data/first_matches.RData")

pnk_Result <- function(Result, pH, pD, pA) {
  p = c(pH, pD, pA)
  p[Result]
}

observed = tab_results[1,]

res = tibble()
for(i in 1:length(predictions_mod_0_dc)) {
  tmp1 = predictions_mod_0_dc[[i]]$Match %>%
    select(-Stoppage_Time_1, -Stoppage_Time_2) %>%
    mutate(Result = ifelse(Score_Home > Score_Away, 1,
                           ifelse(Score_Home == Score_Away, 2, 
                                  3)))
  res = rbind(res, tmp1)
}

res = res %>%
  anti_join(first_matches) %>%
  rowwise() %>%
  mutate(pnk_Result = pnk_Result(Result, observed[1], observed[2], observed[3]))

EnvStats::geoMean(res$pnk_Result) # 0.3509813
