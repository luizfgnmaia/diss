
load("scrape/data/results2.RData")

results = results %>%
  arrange(Date)

teams = unique(results$Home_Team)

tmp = list()
for(i in 1:length(teams)) {
  tmp[[i]] = head(sort(c(which(results$Home_Team == teams[i]), which(results$Away_Team == teams[i]))), 4)
}

tmp = sort(unique(do.call(c, tmp)))

first_matches = results[tmp, ] %>%
  select(-Stoppage_Time_1,-Stoppage_Time_2) %>%
  filter(Season > 2015)

save(first_matches, file = "weight/data/first_matches.RData")





