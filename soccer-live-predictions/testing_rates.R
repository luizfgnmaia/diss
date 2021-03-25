
load("2020/data/mod_1.RData")

home_team = "Flamengo"
away_team = "Internacional"
score_home = 0
score_away = 0
reds_home = 0
reds_away = 0
minute = 44
half = 2

gamma = mod_1$gamma
tau = mod_1$tau
A_lambda_1 = exp(mod_1$a_lambda[1])
A_lambda_2 = exp(mod_1$a_lambda[2])
A_mu_1 = exp(mod_1$a_mu[1])
A_mu_2 = exp(mod_1$a_mu[2])
alpha_i = mod_1$alpha[home_team]
beta_i = mod_1$beta[home_team]
alpha_j = mod_1$alpha[away_team]
beta_j = mod_1$beta[away_team]

lambda = exp(alpha_i+beta_j+gamma+(half==2)*tau)*exp(score_home*mod_1$omega["lambda_x"])*exp(score_away*mod_1$omega["lambda_y"])*exp(reds_home*mod_1$omega["lambda_x^s"])*exp(reds_away*mod_1$omega["lambda_y^s"])
mu = exp(alpha_j+beta_i+(half==2)*tau)*exp(score_home*mod_1$omega["mu_x"])*exp(score_away*mod_1$omega["mu_y"])*exp(reds_home*mod_1$omega["mu_x^s"])*exp(reds_away*mod_1$omega["mu_y^s"])

lambda
mu

1/lambda
1/mu