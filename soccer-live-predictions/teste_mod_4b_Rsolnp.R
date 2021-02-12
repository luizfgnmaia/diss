
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
  
  eta = par[c(2*n+6, 2*n+7)]
  phi = par[c(2*n+8, 2*n+9)]
  kappa = par[2*n+10]
  
  pi1 = eta[1] + phi[1] * g1
  pi2 = eta[2] + phi[2] * g2 + c * kappa
  
  csi = par[c(2*n+11, 2*n+12)]
  
  # f(t) = log(t+1)
  loglambda1 = M1_lambda %*% theta + log((t2_1+1)^(csi[1]+1) - (t1_1+1)^(csi[1]+1)) - log(csi[1]+1)
  logmu1 = M1_mu %*% theta + log((t2_1+1)^(csi[2]+1) - (t1_1+1)^(csi[2]+1)) - log(csi[2]+1)
  loglambda2 = M2_lambda %*% theta + log((t2_2+1)^(csi[1]+1) - (t1_2+1)^(csi[1]+1)) - log(csi[1]+1)
  logmu2 = M2_mu %*% theta + log((t2_2+1)^(csi[2]+1) - (t1_2+1)^(csi[2]+1)) - log(csi[2]+1)
  
  lambda1 = exp(loglambda1)
  mu1 = exp(logmu1)
  lambda2 = exp(loglambda2)
  mu2 = exp(logmu2)
  
  log_lik = - sum(lambda1) - sum(mu1) - sum(lambda2) - sum(mu2) +
    sum(H1*loglambda1) + sum(A1*logmu1) + sum(H2*loglambda2) + sum(A2*logmu2) +
    t(U1) %*% log(pi1) + t(U2) %*% log(pi2) - sum(pi1) - sum(pi2)
  
  print(paste0("loglik: ", log_lik))
  
  -log_lik
}

set.seed(1)
pars = rep(0.5, 2*n+12)
LB = c(rep(-10, 2*n+12))
LB[46:50] = rep(0, 5)

LB[51:52] = rep(-10, 2) # <--- importante
# LB[51:52] = rep(0, 2) # <--- importante

UB = rep(5, 2*n+12)
names(pars) = c(paste0("alpha_", 2:n), paste0("beta_", 1:n), "gamma", "tau", "lambda_10", "lambda_01",
                "mu_10", "mu_01", "eta_1", "eta_2", "phi_1", "phi_2", "kappa", "csi_1", "csi_2")
names(LB) = c(paste0("alpha_", 2:n), paste0("beta_", 1:n), "gamma", "tau", "lambda_10", "lambda_01",
              "mu_10", "mu_01", "eta_1", "eta_2", "phi_1", "phi_2", "kappa", "csi_1", "csi_2")

solution = solnp(pars = pars, fun = log_lik, LB = LB, UB = UB)

duration = Sys.time() - t0

mod_4b_Rsolnp = list(alpha = c(0, solution$pars[1:(n-1)]),
                    beta = solution$pars[n:(2*n-1)],
                    gamma = solution$pars[2*n],
                    tau = solution$pars[2*n+1],
                    lambda_xy = solution$pars[c(2*n+2, 2*n+3)],
                    mu_xy = solution$pars[c(2*n+4, 2*n+5)],
                    csi = solution$pars[c(2*n+11, 2*n+12)],
                    eta = solution$pars[c(2*n+6, 2*n+7)],
                    phi = solution$pars[c(2*n+8, 2*n+9)],
                    kappa = solution$pars[2*n+10],
                    value = -solution$values[length(solution$values)],
                    duration = duration)
names(mod_4b_Rsolnp$alpha) = times$Time
names(mod_4b_Rsolnp$beta) = times$Time
names(mod_4b_Rsolnp$lambda_xy) = c("10", "01")
names(mod_4b_Rsolnp$mu_xy) = c("10", "01")

# save(mod_4b_Rsolnp, file = "mod_4b_Rsolnp.RData")

solution$convergence

exp(mod_4b_Rsolnp$alpha)
exp(mod_4b_Rsolnp$alpha)
exp(mod_4b_Rsolnp$gamma)
exp(mod_4b_Rsolnp$lambda_xy)
exp(mod_4b_Rsolnp$mu_xy)
mod_4b_Rsolnp$eta
mod_4b_Rsolnp$phi
mod_4b_Rsolnp$kappa
mod_4b_Rsolnp$csi



