
library(dplyr)

load("dados_serie_a_2019.RData")

#### T1
gols_t1 = unlist(lapply(t1, function(x) length(x < 45)))
reds_t1 = unlist(lapply(t1s, function(x) length(x < 45)))
fit_T1 = glm(T1 ~ gols_t1 + reds_t1, family = "poisson")
summary(fit_T1)

#### T2
gols_t2 = unlist(lapply(t2, function(x) length(x < 90)))
reds_t2 = unlist(lapply(t2s, function(x) length(x < 90)))
fit_T2 = glm(T2 ~ gols_t2 + reds_t2, family = "poisson")
summary(fit_T2)

library(ggplot2)

tibble(x = T1) %>%
  ggplot(aes(x = x))  +
  geom_bar() +
  scale_x_continuous(breaks = 0:9) +
  theme_bw() +
  xlab("") +
  ggtitle("T1")

tibble(x = T2) %>%
  ggplot(aes(x = x))  +
  geom_bar() +
  scale_x_continuous(breaks = 0:12) +
  theme_bw() +
  xlab("") +
  ggtitle("T2")

# tibble(x = fit_T1$fitted.values) %>%
#   ggplot(aes(x = x))  +
#   geom_bar() +
#   xlim(0,9) +
#   theme_bw() +
#   xlab("") +
#   ggtitle("T1")
# 
# tibble(x = fit_T2$fitted.values) %>%
#   ggplot(aes(x = x))  +
#   geom_bar() +
#   xlim(0, 12) +
#   theme_bw() +
#   xlab("") +
#   ggtitle("T2")

tibble(x = rpois(length(T1), fit_T1$fitted.values)) %>%
  ggplot(aes(x = x)) +
  geom_bar() +
  theme_bw() +
  xlab("") +
  ggtitle("T1")

tibble(x = rpois(length(T2), fit_T2$fitted.values)) %>%
  ggplot(aes(x = x)) +
  geom_bar() +
  theme_bw() +
  xlab("") +
  ggtitle("T2")

# rm(list = setdiff(ls(), c("fit_T1", "fit_T2")))
# save.image(file = "fit_T.RData")




