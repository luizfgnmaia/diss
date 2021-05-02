
load("scrape/data/results.RData")

results$Home_Team[which(results$Home_Team == "América - MG")] = "América-MG"
results$Home_Team[which(results$Home_Team == "Athletico Paranaense - PR")] = "Athletico-PR"
results$Home_Team[which(results$Home_Team == "Atlético - GO")] = "Atlético-GO"
results$Home_Team[which(results$Home_Team == "Atlético - MG")] = "Atlético-MG"
results$Away_Team[which(results$Away_Team == "América - MG")] = "América-MG"
results$Away_Team[which(results$Away_Team == "Athletico Paranaense - PR")] = "Athletico-PR"
results$Away_Team[which(results$Away_Team == "Atlético - GO")] = "Atlético-GO"
results$Away_Team[which(results$Away_Team == "Atlético - MG")] = "Atlético-MG"

results = results %>%
  mutate(Home_Team = stringr::str_replace_all(Home_Team, "\\s-\\s.*", ""),
         Away_Team = stringr::str_replace_all(Away_Team, "\\s-\\s.*", ""))

save(results, file = "scrape/data/results2.RData")
  