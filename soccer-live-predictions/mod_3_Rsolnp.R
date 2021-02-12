
library(dplyr)
library(Rsolnp)

load("dados_serie_a_2019.RData")
load("dados_mod_3.RData")

t0 = Sys.time()

log_lik <- function(par) {
  
  print(par)
  alpha = par[1:(n-1)]
  beta = par[n:(2*n-1)]
  gamma = par[2*n]
  tau = par[2*n+1]
  lambda_xy = par[c(2*n+2, 2*n+3)]
  mu_xy = par[c(2*n+4, 2*n+5)]
  theta = c(alpha, beta, gamma, tau, lambda_xy, mu_xy)
  
  eta = par[c(2*n+6, 2*n+7)]
  phi = par[c(2*n+8, 2*n+9)]
  kappa = par[2*n+10]
  
  pi1 = eta[1] + phi[1] * g1
  pi2 = eta[2] + phi[2] * g2 + c * kappa
  
  as.numeric(
    (-1) * 
      (-t(delta1)%*%exp(M1_lambda%*%theta) -t(delta1)%*%exp(M1_mu%*%theta) -t(delta2)%*%exp(M2_lambda%*%theta) -t(delta2)%*%exp(M2_mu%*%theta) +
         t(H1)%*%M1_lambda%*%theta + t(A1)%*%M1_mu%*%theta + t(H2)%*%M2_lambda%*%theta + t(A2)%*%M2_mu%*%theta +
         t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum(pi1) - sum(pi2))
  )
}

set.seed(1)
pars = rep(0.5, 2*n+12)
LB = c(rep(-10, 2*n+12))
LB[46:50] = rep(0, 5)
UB = rep(5, 2*n+12)
solution = solnp(pars = pars, fun = log_lik, LB = LB, UB = UB)

duration = Sys.time() - t0

mod_3_Rsolnp = list(alpha = c(0, solution$pars[1:(n-1)]),
             beta = solution$pars[n:(2*n-1)],
             gamma = solution$pars[2*n],
             tau = solution$pars[2*n+1],
             lambda_xy = solution$pars[c(2*n+2, 2*n+3)],
             mu_xy = solution$pars[c(2*n+4, 2*n+5)],
             eta = solution$pars[c(2*n+6, 2*n+7)],
             phi = solution$pars[c(2*n+8, 2*n+9)],
             kappa = solution$pars[2*n+10],
             value = -solution$values[length(solution$values)],
             duration = duration)
names(mod_3_Rsolnp$alpha) = times$Time
names(mod_3_Rsolnp$beta) = times$Time
names(mod_3_Rsolnp$lambda_xy) = c("10", "01")
names(mod_3_Rsolnp$mu_xy) = c("10", "01")

save(mod_3_Rsolnp, file = "mod_3_Rsolnp.RData")

mod_3_Rsolnp

solution$convergence