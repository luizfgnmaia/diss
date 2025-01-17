
library(dplyr)
library(ggplot2)

load("weight/data/mod_12_dc.RData")

y = NULL
x = as.Date(names(mod_12_dc), format = "%Y-%m-%d")
for(i in 1:length(mod_12_dc)) {
  y[i] = exp(mod_12_dc[[i]]$gamma)
}

p = tibble(x = x, y = y) %>%
  ggplot(aes(x, y)) +
  geom_line() +
  theme_bw() +
  geom_point() +
  xlab("Data") +
  ylab(expression(gamma))
p

ggsave(filename = paste0("weight/plots/gamma.png"),
       plot = p, width = 10, height = 5, dpi = 1000)

load("scrape/data/results.Rdata")
results = results %>%
  mutate(Result = ifelse(Score_Home > Score_Away, 1,
                         ifelse(Score_Home == Score_Away, 2, 
                                3)))

min(y)
max(y)

results %>%
  group_by(Season) %>%
  count(Result)

# Em 2017, as equipes mandantes venceram apenas 167 partidas (0,4395)
# Em 2020, as equipes mandantes venceram apenas 171 partidas (0,45)
# Considerando todos os anos, a média é de 49,41%
# Gamma variou entre 1,5337 e 1,7066
