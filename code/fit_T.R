
library(dplyr)

load("dados_serie_a_2019.RData")

#### T1
gols_t1 = unlist(lapply(t1, function(x) length(x < 45)))
reds_t1 = unlist(lapply(t1s, function(x) length(x < 45)))
fit_T1 = glm(T1 ~ gols_t1 + reds_t1)
summary(fit_T1)

#### T2
gols_t2 = unlist(lapply(t2, function(x) length(x < 90)))
reds_t2 = unlist(lapply(t2s, function(x) length(x < 90)))
fit_T2 = glm(T2 ~ gols_t2 + reds_t2)
summary(fit_T2)

library(ggplot2)

tibble(x = T1) %>%
  ggplot(aes(x = x)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "firebrick2", col = "black") +
  geom_density(data = tibble(x = fitted.values(fit_T1)), size = 1) +
  theme_bw() +
  ylab("density") +
  xlab("") +
  scale_x_continuous(breaks = 0:9) +
  ggtitle("T1")

tibble(x = T2) %>%
  ggplot(aes(x = x)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "firebrick2", col = "black") +
  geom_density(data = tibble(x = fitted.values(fit_T2)), size = 1) +
  theme_bw() +
  ylab("density") +
  xlab("") +
  scale_x_continuous(breaks = 0:12) +
  ggtitle("T2")

tibble(x = T2) %>%
  ggplot(aes(x = x)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "firebrick2", col = "black") +
  #geom_density(data = tibble(x = fitted.values(fit_T2)), size = 1) +
  theme_bw() +
  #ylab("density") +
  xlab("") +
  scale_x_continuous(breaks = 0:12) +
  ggtitle("T2")

rm(list = setdiff(ls(), c("fit_T1", "fit_T2")))
save.image(file = "fit_T.RData")

