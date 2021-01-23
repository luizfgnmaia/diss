
library(Rsolnp)

load("serie_a_2019.RData")

log_lik <- function(par) { 
  alpha = par[1:20]
  beta = par[21:40]
  gamma = par[41]
  lambda = alpha[i] * beta[j] * gamma
  mu = alpha[j] * beta[i]
  -sum(-lambda + x*log(lambda) - mu + y*log(mu))
}

set.seed(1)
mod_0_Rsolnp = solnp(pars = rep(1, 41), log_lik, LB = rep(0, 41), eqfun = function(par) par[1], eqB = 1)
save(mod_0_Rsolnp, file = "sol/mod_0_Rsolnp.RData")
