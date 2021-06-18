
source("weight/fit_mod_C.R")

begin = Sys.time()

mod_C_dc = fit_mod_C_dates(0.0065/3.5)

duration = Sys.time() - begin

duration # Time difference of 33.28491 mins

save(mod_C_dc, file = "weight/data/mod_C_dc.RData")


