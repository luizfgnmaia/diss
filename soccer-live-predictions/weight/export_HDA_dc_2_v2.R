load("C:/Users/Luiz/Documents/GitHub/soccer-live-predictions/soccer-live-predictions/weight/data/HDA_dc_2_v2.RData")


library(stringr)

names(HDA_dc) = str_replace_all(names(HDA_dc), "mod_B_", "mod_1_")
names(HDA_dc) = str_replace_all(names(HDA_dc), "mod_C_", "mod_2_")
names(HDA_dc) = str_replace_all(names(HDA_dc), "mod_12_", "mod_4_")

write.csv2(HDA_dc, file = "partidas_e_previsoes.csv", row.names = FALSE)
