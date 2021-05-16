
source("weight/fit_mod_9.R")

begin = Sys.time()

mod_9_dc = fit_mod_9_dates(0.0065/3.5)

duration = Sys.time() - begin

duration # Time difference of 58.63947 mins

save(mod_9_dc, file = "weight/data/mod_9_dc.RData")


