
load("transfermarkt/data/results.RData")

# https://www.transfermarkt.com.br/cr-flamengo/startseite/verein/614?saison_id=2019
# https://www.google.com/search?q=flamengo+vs+sao+paulo+2021+38+rodada&oq=flamengo+vs+sao+paulo+2021+38+rodada&aqs=chrome..69i57j0i22i30l2.8071j0j4&sourceid=chrome&ie=UTF-8#sie=m;/g/11j8tnwwd9;2;/m/0fnk7q;ln;fp;1;;
fla_2020 = c(5200000, # B. Henrique
             18500000, # Gabriel
             15000000, # G. De Arrascaeta
             2800000, # Diego
             11000000, # Gerson
             6500000, # E. Ribeiro
             2000000, # F. Luís
             2400000, # G. Henrique
             4800000, # R. Caio
             950000, # M. Isla
             450000) # Hugo Souza
sum(fla_2020)
# 79050000

# https://www.transfermarkt.com.br/cr-flamengo/startseite/verein/614?saison_id=2020
fla_2021 = c(4500000, # B. Henrique
             26000000, # Gabriel
             18000000, # G. De Arrascaeta
             1300000, # Diego
             20000000, # Gerson
             8000000, # E. Ribeiro
             1500000, # F. Luís
             2600000, # G. Henrique
             6500000, # R. Caio
             950000, # M. Isla
             3500000) # Hugo Souza
sum(fla_2021)
# 79050000

