
library(dplyr)
library(CVXR)

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

# CVXR

log_lik <- function(alpha, beta, gamma) {
  lambda = alpha[i] + beta[j] + gamma
  mu = alpha[j] + beta[i]
  sum(-exp(lambda) + x*lambda - exp(mu) + y*mu)
}

alpha = Variable(20)
beta = Variable(20)
gamma = Variable(1)

objective = Maximize(log_lik(alpha, beta, gamma))
problem = Problem(objective) # não consegui incluir a constraint, fazer regra de 3 no fim
set.seed(1)
solution = solve(problem)

sol_alpha = exp(solution$getValue(alpha))
sol_beta = exp(solution$getValue(beta))
sol_gamma = exp(solution$getValue(gamma))

prop = n/sum(sol_alpha)
sol_alpha = sol_alpha*prop
sol_beta = sol_beta/prop

sol_alpha
sol_beta
sol_gamma

sol_modelo_0_cvxr = solution
save(sol_modelo_0_cvxr, file = "sol/modelo_0_CVXR.RData")


