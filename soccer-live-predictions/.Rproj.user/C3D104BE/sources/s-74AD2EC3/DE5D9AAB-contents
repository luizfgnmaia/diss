
library(Rsolnp)

load("2019/data/input.RData")
load("2019/data/input_mod_3.RData")

t0 = Sys.time()

log_lik <- function(pars) {
  
  print(pars)
  alpha = pars[1:n]
  beta = pars[(n+1):(2*n)]
  gamma = pars[2*n+1]
  tau = pars[2*n+2]
  lambda_xy = pars[c(2*n+3, 2*n+4)]
  mu_xy = pars[c(2*n+5, 2*n+6)]
  theta = c(alpha, beta, gamma, tau, lambda_xy, mu_xy)
  
  eta = pars[c(2*n+7, 2*n+8)]
  phi = pars[c(2*n+9, 2*n+10)]
  kappa = pars[2*n+11]
  
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
pars = rep(0.5, 2*n+11)
LB = c(rep(-10, 2*n+11))
LB[47:51] = rep(0, 5)
UB = rep(5, 2*n+11)

eqfun <- function(pars) {
  sum(pars[1:20]) - sum(pars[21:40])
}

solution = solnp(pars = pars, fun = log_lik, LB = LB, UB = UB, eqfun = eqfun, eqB = 0)

duration = Sys.time() - t0

mod_3_Rsolnp = list(alpha = c(solution$pars[1:n]),
             beta = solution$pars[(n+1):(2*n)],
             gamma = solution$pars[2*n+1],
             tau = solution$pars[2*n+2],
             lambda_xy = solution$pars[c(2*n+3, 2*n+4)],
             mu_xy = solution$pars[c(2*n+5, 2*n+6)],
             eta = solution$pars[c(2*n+7, 2*n+8)],
             phi = solution$pars[c(2*n+9, 2*n+10)],
             kappa = solution$pars[2*n+11],
             value = -solution$values[length(solution$values)],
             duration = duration)
names(mod_3_Rsolnp$alpha) = times$Time
names(mod_3_Rsolnp$beta) = times$Time
names(mod_3_Rsolnp$lambda_xy) = c("10", "01")
names(mod_3_Rsolnp$mu_xy) = c("10", "01")

save(mod_3_Rsolnp, file = "2019/data/mod_3_Rsolnp.RData")

mod_3_Rsolnp

solution$convergence
