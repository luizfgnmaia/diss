
source("weight/fit_mod_10.R")

begin = Sys.time()

mod_10_dc = fit_mod_10_dates(0.0065/3.5)

duration = Sys.time() - begin

duration # Time difference of 58.98715 mins 

save(mod_10_dc, file = "weight/data/mod_10_dc.RData")


