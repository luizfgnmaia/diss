
library(dplyr)
library(ggplot2)

load("weight/data/mod_8_dc.RData")

y = NULL
x = as.Date(names(mod_8_dc), format = "%Y-%m-%d")
for(i in 1:length(mod_8_dc)) {
  y[i] = mod_8_dc[[i]]$alpha["Red Bull Bragantino"]
}

tibble(x = x, y = y) %>%
  ggplot(aes(x, y)) +
  geom_line() +
  theme_bw() +
  geom_point()

names(y) = x

which(y == max(y))
# 2019-05-01 
# 313 