
source("weight/fit_mod_3.R")

begin = Sys.time()

mod_3_dc = fit_mod_3_dates(0.0065/3.5)

duration = Sys.time() - begin

duration # Time difference of 1.575636 hours

save(mod_3_dc, file = "weight/data/mod_3_dc.RData")

# Obs: O MOSEK apresentou erro em "2021-02-15" (i = 513) retornando status == "solver_error",
# coloquei para a função rodar o "ECOS" caso isso aconteça e printar o dia em que isso aconteceu,


