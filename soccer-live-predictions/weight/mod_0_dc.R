
source("weight/fit_mod_0.R")

begin = Sys.time()

mod_0_dc = fit_mod_0_dates(0.0065/3.5)

duration = Sys.time() - begin

duration # Time difference of 4.829664 mins

save(mod_0_dc, file = "weight/data/mod_0_dc.RData")



