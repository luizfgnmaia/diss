
# No jogo 65 da série B de 2018, ocorreu um gol aos 90+8 minutos e o 
# jogo teve apenas 4 minutos de acréscimos.

# Em 106 jogos entre 2014 e 2019, ocorreu um gol no último minuto de um 
# tempo mas acredito que isso não dê problema.


library(dplyr)

load("data/resultados.RData")
load("data/gols.RData")

# Primeiro tempo
t1 = gols %>%
  rename(Acréscimo_Gol = Acréscimo) %>%
  filter(!is.na(Acréscimo_Gol),
         Tempo == "1º",
         Ano >= 2014) %>%
  select("Campeonato", "Ano", "Jogo", "Tempo", "Acréscimo_Gol") %>%
  left_join(resultados) %>%
  rowwise() %>%
  mutate(problema = Acréscimo_Gol >= Acréscimos_1) %>%
  filter(problema == TRUE)
View(t1)

# Segundo tempo
t2 = gols %>%
  rename(Acréscimo_Gol = Acréscimo) %>%
  filter(!is.na(Acréscimo_Gol),
         Tempo == "2º",
         Ano >= 2014) %>%
  select("Campeonato", "Ano", "Jogo", "Tempo", "Acréscimo_Gol") %>%
  left_join(resultados) %>%
  rowwise() %>%
  mutate(problema = Acréscimo_Gol >= Acréscimos_2) %>%
  filter(problema == TRUE)
View(t2)