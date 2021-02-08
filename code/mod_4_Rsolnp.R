
library(dplyr)
library(Rsolnp)

load("dados_serie_a_2019.RData")
load("dados_mod_4.RData")

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
  
  csi = par[c(2*n+11, 2*n+12)]
  
  eta = par[c(2*n+6, 2*n+7)]
  phi = par[c(2*n+8, 2*n+9)]
  kappa = par[2*n+10]
  
  pi1 = eta[1] + phi[1] * g1
  pi2 = eta[2] + phi[2] * g2 + c * kappa
  
  # # Mandante primeiro tempo
  # f_t_csi_1_1 = ((t2_1+1)^(csi[1]) - (t1_1+1)^(csi[1])) / (csi[1]+1) 
  # # Visitante primeiro tempo
  # f_t_csi_2_1 = ((t2_1+1)^(csi[2]) - (t1_1+1)^(csi[1])) / (csi[2]+1) 
  # # Mandante segundo tempo
  # f_t_csi_1_2 = ((t2_2+1)^(csi[1]) - (t1_2+1)^(csi[1])) / (csi[1]+1) 
  # # Visitante segundo tempo
  # f_t_csi_2_2 = ((t2_2+1)^(csi[2]) - (t1_2+1)^(csi[1])) / (csi[2]+1) 
  
  # Mandante primeiro tempo
  f_t_csi_1_1 = (exp(t2_1*csi[1]) - exp(t1_1*csi[1])) / csi[1]
  # Visitante primeiro tempo
  f_t_csi_2_1 = (exp(t2_1*csi[2]) - exp(t1_1*csi[2])) / csi[2]
  # Mandante segundo tempo
  f_t_csi_1_2 = (exp(t2_2*csi[1]) - exp(t1_2*csi[1])) / csi[1]
  # Visitante segundo tempo
  f_t_csi_2_2 = (exp(t2_2*csi[2]) - exp(t1_2*csi[2])) / csi[2]

  lambda1 = exp(M1_lambda %*% theta) * f_t_csi_1_1
  mu1 = exp(M1_mu %*% theta) * f_t_csi_2_1
  lambda2 = exp(M2_lambda %*% theta) * f_t_csi_1_2
  mu2 = exp(M2_mu %*% theta) * f_t_csi_2_2
  
  loglambda1 = M1_lambda %*% theta + log(f_t_csi_1_1)
  logmu1 = M1_mu %*% theta + log(f_t_csi_2_1)
  loglambda2 = M2_lambda %*% theta + log(f_t_csi_1_2)
  logmu2 = M2_mu %*% theta + log(f_t_csi_2_2)
  
  # Aparecem NAs quando t1 = t2 daí log(0) = -Inf, posso só ignorar esses intervalos?
  as.numeric(
    (-1) *( - sum(lambda1) - sum(mu1) - sum(lambda2) - sum(mu2) +
              sum(H1*loglambda1, na.rm = TRUE) + sum(A1*logmu1, na.rm = TRUE) + sum(H2*loglambda2, na.rm = TRUE) + sum(A2*logmu2, na.rm = TRUE) +
              t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum(pi1) - sum(pi2))
  )
}

set.seed(1)
solution = solnp(pars = rep(1, 2*n+12), log_lik)

duration = Sys.time() - t0

mod_4_Rsolnp = list(alpha = c(0, solution$pars[1:(n-1)]),
                    beta = solution$pars[n:(2*n-1)],
                    gamma = solution$pars[2*n],
                    tau = solution$pars[2*n+1],
                    lambda_xy = solution$pars[c(2*n+2, 2*n+3)],
                    mu_xy = solution$pars[c(2*n+4, 2*n+5)],
                    eta = solution$pars[c(2*n+6, 2*n+7)],
                    phi = solution$pars[c(2*n+8, 2*n+9)],
                    kappa = solution$pars[2*n+10],
                    csi = solution$pars[c(2*n+11, 2*n+12)],
                    value = -solution$values[length(solution$values)],
                    duration = duration)
names(mod_4_Rsolnp$alpha) = times$Time
names(mod_4_Rsolnp$beta) = times$Time
names(mod_4_Rsolnp$lambda_xy) = c("10", "01")
names(mod_4_Rsolnp$mu_xy) = c("10", "01")

# save(mod_4_Rsolnp, file = "mod_4_Rsolnp.RData")

