
source("weight/fit_mod_12.R")

begin = Sys.time()

mod_12_dc = fit_mod_12_dates(0.0065/3.5)

duration = Sys.time() - begin

duration # Time difference of 51.31187 mins

save(mod_12_dc, file = "weight/data/mod_12_dc.RData")


