
library(dplyr)
library(Rsolnp)

# Preparando o data set

load("data/resultados.RData")
load("data/gols.RData")

resultados = resultados %>%
  filter(Campeonato == "Campeonato Brasileiro Série A",
         Ano == 2019) %>%
  select(-Campeonato, -Ano, -Jogo, -Data)

times = tibble(Id = 1:20, Time = sort(unique(resultados$Time_1)))

tmp1 = times %>%
  rename(Time_1 = Time,
         i = Id)

tmp2 = times %>%
  rename(Time_2 = Time,
         j = Id)

resultados = resultados %>%
  inner_join(tmp1) %>%
  inner_join(tmp2) %>%
  rename(x = Placar_1,
         y = Placar_2) %>%
  mutate(k = 1:nrow(.)) %>%
  select(k, i, j, x, y)

i = resultados$i; j = resultados$j

N = nrow(resultados); n = nrow(times)

gols = gols %>%
  filter(Campeonato == "Campeonato Brasileiro Série A",
         Ano == 2019) %>%
  mutate(Minuto = ifelse(Tempo == "1º", Minuto, Minuto + 45),
         t = Minuto/90,
         J = ifelse(Time == "Visitante", 1, 0))

lst_t = list()
lst_J = list()
lst_x = list()
lst_y = list()
for(k in 1:N) { # otimizar essa parte?
  jogo = gols %>%
    filter(Jogo == k)
  if(nrow(jogo) > 0) {
    lst_t[[k]] = jogo$t
    lst_J[[k]] = jogo$J
    tmp_x = rep(0, 91) # primeira entrada é o minuto 0
    tmp_y = rep(0, 91)
    mandante = jogo %>%
      filter(J == 0)
    visitante = jogo %>%
      filter(J == 1)
    if(nrow(mandante > 0)) {
      for(m in 1:nrow(mandante)) {
        tmp_x[(mandante$Minuto[m]+1):91] = tmp_x[mandante$Minuto[m]]+1
      }
    }
    if(nrow(visitante > 0)) {
      for(m in 1:nrow(visitante)) {
        tmp_y[(visitante$Minuto[m]+1):91] = tmp_y[visitante$Minuto[m]]+1
      }
    }
    lst_x[[k]] = tmp_x
    lst_y[[k]] = tmp_y
  } else {
    lst_t[[k]] = NA
    lst_J[[k]] = NA
    lst_x[[k]] = rep(0, 91)
    lst_y[[k]] = rep(0, 91)
  }
}

# Função de máxima verossimilhança

log_lik <- function(par) {
  # par = alpha_1, ... alpha_20,                        par[1:20]
  #       beta_1, ..., beta_20,                         par[21:40]
  #       gamma,                                        par[41]
  #       lambda_10, lambda_01, lambda_21, lambda_12    par[42:45]
  #       mu_10, mu_01, mu_21, mu_12                    par[46:49]
  #       ro_1, ro_2                                    par[50:51]
  #       csi_1, csi_2                                  par[52:53]
  
  # Funções auxiliares
  
  log_lik_k <- function(k) { # Verossimilhança para uma partida k

    lambda_xy <- function(t) {
      x = lst_x[[k]][t+1]
      y = lst_y[[k]][t+1]
      ifelse(x==1 & y==0, lambda_10,
             ifelse(x==0 & y==1, lambda_01,
                    ifelse(x+y>1 & x-y>=1, lambda_21,
                           ifelse(x+y>1 & x-y<=-1, lambda_12, 1))))
    }
    
    mu_xy <- function(t) {
      x = lst_x[[k]][t+1]
      y = lst_y[[k]][t+1]
      ifelse(x==1 & y==0, mu_10,
             ifelse(x==0 & y==1, mu_01,
                    ifelse(x+y>1 & x-y>=1, mu_21,
                           ifelse(x+y>1 & x-y<=-1, mu_12, 1))))
    }
    
    lambda_k <- function(t) {
      ro_1^(t==45)*ro_2^(t==90)*lambda_xy(t)*gamma*alpha[i[k]]*beta[j[k]]+csi_1*(t/90)
    }
    
    mu_k <- function(t) {
      ro_1^(t==45)*ro_2^(t==90)*mu_xy(t)*alpha[j[k]]*beta[i[k]]+csi_2*(t/90)
    }
    
    int_lambda <- function(t1, t2) {
      integrate(lambda_k, lower = t1/90, upper = t2/90)$value
    }
    
    int_mu <- function(t1, t2) {
      integrate(mu_k, lower = t1/90, upper = t2/90)$value
    }
    
    xy = paste0(lst_x[[k]], "-", lst_y[[k]])
    tab = table(xy)
    if(length(tab) > 1) {
      acum = c(0, cumsum(tab)-1)
      inicio = acum[-length(acum)]
      fim = acum[-1]
    } else {
      inicio = 0
      fim = 90
    }
    soma_int_lambda = 0
    soma_int_mu = 0
    for(int in 1:length(inicio)) {
      soma_int_lambda = soma_int_lambda + int_lambda(t1 = inicio[int], t2 = fim[int])
      soma_int_mu = soma_int_mu + int_mu(t1 = inicio[int], t2 = fim[int])
    }
    mk = ifelse(is.na(lst_J[[k]][1]), 0, sum(lst_J[[k]]))
    if(mk > 0) {
      soma_l_mk = sum(log(lambda_k(lst_t[[k]])^(1-lst_J[[k]]) * mu_k(lst_t[[k]])^(lst_J[[k]])))
    } else {
      soma_l_mk = 0
    }
    soma_l_mk - soma_int_lambda - soma_int_mu
  }
  
  alpha = par[1:20]
  beta = par[21:40]
  gamma = par[41]
  lambda_10 = par[42]
  lambda_01 = par[43]
  lambda_21 = par[44]
  lambda_12 = par[45]
  mu_10 = par[46]
  mu_01 = par[47]
  mu_21 = par[48]
  mu_12 = par[49]
  ro_1 = par[50]
  ro_2 = par[51]
  csi_1 = par[52]
  csi_2 = par[53]
  soma = 0
  for(k in 1:N) {
    soma = soma + log_lik_k(k)
  }
  -soma
}

# solnp

eqfun <- function(par) {
  sum(par[1:20])
}

set.seed(1)
sol = solnp(pars = rep(1, 53), log_lik, LB = c(rep(0, 41), rep(-Inf, 12)), eqfun = eqfun, eqB = n)

pars = tibble(Parâmetro = c(paste0("alpha_", 1:20), paste0("beta_", 1:20), "gamma",
                            "lambda_10", "lambda_01", "lambda_21", "lambda_12", "mu_10", "mu_01", 
                            "mu_21", "mu_12", "ro_1", "ro_2", "csi_1", "cs1_2"),
              Valor = round(sol$pars, 6))

View(pars)
