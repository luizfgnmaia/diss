
source("weight/fit_mod_3.R")

begin = Sys.time()

dixon_coles_mod_3 = fit_mod_3_dates(0.0065/3.5)

duration = Sys.time() - begin

duration # Time difference of 1.576407 hours

save(dixon_coles_mod_3, file = "weight/data/dixon_coles_mod_3.RData")

# Obs: O MOSEK apresentou erro em "2021-02-15" (i = 513) retornando status == "solver_error",
# coloquei para a função rodar o "ECOS" caso isso aconteça e printar o dia em que isso aconteceu,


