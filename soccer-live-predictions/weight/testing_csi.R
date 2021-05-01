
load("scrape/data/results.RData")

range = as.integer(difftime(as.Date(max(results$Date), "%Y-%m-%d"), as.Date(min(results$Date), "%Y-%m-%d"), units = "days"))

csi = 0.0065 / 3.5
weigth = exp(-csi*range:1)
summary(weigth)
plot(1:range, weigth)


