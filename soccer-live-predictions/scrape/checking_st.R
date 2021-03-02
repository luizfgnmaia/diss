
library(dplyr)

load("scrape/data/results.RData")
load("scrape/data/goals.RData")
load("scrape/data/reds.RData")

goals = goals %>%
  inner_join(results)

goals %>%
  filter(Half == 1) %>%
  rowwise() %>%
  mutate(problem = Stoppage_Time > Stoppage_Time_1) %>% 
  filter(problem == TRUE)

goals %>%
  filter(Half == 2) %>%
  rowwise() %>%
  mutate(problem = Stoppage_Time > Stoppage_Time_2) %>% 
  filter(problem == TRUE)

reds = reds %>%
  inner_join(results)

reds %>%
  filter(Half == 1) %>%
  rowwise() %>%
  mutate(problem = Stoppage_Time > Stoppage_Time_1) %>% 
  filter(problem == TRUE)

reds %>%
  filter(Half == 2) %>%
  rowwise() %>%
  mutate(problem = Stoppage_Time > Stoppage_Time_2) %>% 
  filter(problem == TRUE)



