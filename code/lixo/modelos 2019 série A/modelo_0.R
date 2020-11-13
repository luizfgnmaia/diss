
library(dplyr)
library(Rsolnp)

# Modelo

# X_ij ~ Poisson(alpha_i * beta_j * gamma_h)
# Y_ij ~ Poisson(alpha_j * beta_i)

# Preparando o data set

load("data/resultados.RData")

dados = resultados %>%
  filter(Campeonato == "Campeonato Brasileiro Série A",
         Ano == 2019) %>%
  select(-Campeonato, -Ano, -Jogo, -Data)

times = tibble(Id = 1:20, Time = sort(unique(dados$Time_1)))

tmp1 = times %>%
  rename(Time_1 = Time,
         i = Id)

tmp2 = times %>%
  rename(Time_2 = Time,
         j = Id)

dados = dados %>%
  inner_join(tmp1) %>%
  inner_join(tmp2) %>%
  rename(x = Placar_1,
         y = Placar_2)

i = dados$i; j = dados$j; x = dados$x; y = dados$y

N = nrow(dados); n = nrow(times)

# solnp

log_lik <- function(par) { # par = (alpha_1, ..., alpha_20, beta_1, ... beta_20, gamma_h)
  alpha = par[1:20]
  beta = par[21:40]
  gamma = par[41]
  lambda = alpha[i] * beta[j] * gamma
  mu = alpha[j] * beta[i]
  -sum(-lambda + x*log(lambda) - mu + y*log(mu))
}

eqfun <- function(par) {
  sum(par[1:20])
}

set.seed(1)
sol = solnp(pars = rep(1, 41), log_lik, LB = c(rep(0, 40), -Inf), eqfun = eqfun, eqB = n)

sol_modelo_0 = sol
save(sol_modelo_0, file = "sol/sol_modelo_0.RData")


                 