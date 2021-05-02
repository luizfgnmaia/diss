
load("2015-2020/data/input.RData")

ind = copy_results %>%
  filter(Season > 2015) %>%
  .$ind

save(ind, file = "weight/data/matches_to_be_predicted.RData")
