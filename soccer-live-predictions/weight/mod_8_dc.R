
source("weight/fit_mod_8.R")

begin = Sys.time()

mod_8_dc = fit_mod_8_dates(0.0065/3.5)

duration = Sys.time() - begin

duration # Time difference of 1.55327 hours

save(mod_8_dc, file = "weight/data/mod_8_dc.RData")


