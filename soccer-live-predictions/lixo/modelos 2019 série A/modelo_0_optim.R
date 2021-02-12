
library(dplyr)

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

log_lik <- function(par) { # par = (alpha_1, ..., alpha_19, beta_1, ... beta_20, gamma_h)
  alpha = par[1:19]
  alpha[20] = n - sum(alpha)
  beta = par[20:39]
  gamma = par[40]
  lambda = alpha[i] * beta[j] * gamma
  mu = alpha[j] * beta[i]
  -sum(-lambda + x*log(lambda) - mu + y*log(mu))
}

set.seed(1)
sol = optim(par = rep(1, 40), fn = log_lik, lower = 0, method = "L-BFGS-B")

param = tibble(Time = stringr::str_replace_all(times$Time, "(?=\\s- ).*", ""),
               Alpha = c(sol$par[1:19], n - sum(sol$par[1:19])),
               Beta = sol$par[20:39])

gamma = sol$par[40]

View(param)
print(gamma)



