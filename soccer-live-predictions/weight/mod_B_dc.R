
source("weight/fit_mod_B.R")

begin = Sys.time()

mod_B_dc = fit_mod_B_dates(0.0065/3.5)

duration = Sys.time() - begin

duration # Time difference of 32.81008 mins

save(mod_B_dc, file = "weight/data/mod_B_dc.RData")


