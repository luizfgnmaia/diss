
library(dplyr)

load("dados_serie_a_2019.RData")

#### U1
gols_t1 = unlist(lapply(t1, function(x) length(x < 45)))
reds_t1 = unlist(lapply(t1s, function(x) length(x < 45)))
fit_U1 = glm(U1 ~ gols_t1 + reds_t1, poisson(link = "log"))
summary(fit_U1)
exp(fit_U1$coefficients)


#### U2
close_game = NULL
for(k in 1:N) {
  close_game[k] = as.integer(abs(x2[[k]][46] - y2[[k]][46]) <= 1)
}

gols_t2 = unlist(lapply(t2, function(x) length(x < 90)))
reds_t2 = unlist(lapply(t2s, function(x) length(x < 90)))
fit_U2 = glm(U2 ~ gols_t2 + reds_t2 + close_game, poisson(link = "log"))
summary(fit_U2)
exp(fit_U2$coefficients)

library(ggplot2)

tibble(x = U1) %>%
  ggplot(aes(x = x))  +
  geom_bar() +
  scale_x_continuous(breaks = 0:9) +
  theme_bw() +
  xlab("") +
  ggtitle("U1")

tibble(x = U2) %>%
  ggplot(aes(x = x))  +
  geom_bar() +
  scale_x_continuous(breaks = 0:12) +
  theme_bw() +
  xlab("") +
  ggtitle("U2")

# tibble(x = fit_U1$fitted.values) %>%
#   ggplot(aes(x = x))  +
#   geom_bar() +
#   xlim(0,9) +
#   theme_bw() +
#   xlab("") +
#   ggtitle("U1")
# 
# tibble(x = fit_U2$fitted.values) %>%
#   ggplot(aes(x = x))  +
#   geom_bar() +
#   xlim(0, 12) +
#   theme_bw() +
#   xlab("") +
#   ggtitle("U2")

tibble(x = rpois(length(U1), fit_U1$fitted.values)) %>%
  ggplot(aes(x = x)) +
  geom_bar() +
  theme_bw() +
  xlab("") +
  ggtitle("U1")

tibble(x = rpois(length(U2), fit_U2$fitted.values)) %>%
  ggplot(aes(x = x)) +
  geom_bar() +
  theme_bw() +
  xlab("") +
  ggtitle("U2")

# rm(list = setdiff(ls(), c("fit_U1", "fit_U2")))
# save.image(file = "fit_T.RData")




