
library(dplyr)

load("data/resultados.RData")
load("data/gols.RData")

resultados = resultados %>%
  na.omit()

sum(resultados$Placar_1) + sum(resultados$Placar_2)
nrow(gols)

# Problemas entre 2014 e 2019 SÉRIE A:

# Jogo 371 de 2019
# https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2019/371
# https://conteudo.cbf.com.br/sumulas/2019/142371se.pdf
# Consta 2x1 no site, 0x0 na súmula e não tem os gols no site
# https://www.uol.com.br/esporte/futebol/ultimas-noticias/2019/12/08/internacional-x-atletico-mg.htm

# Jogo 292 de 2016
# https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2016/292
# https://conteudo.cbf.com.br/sumulas/2016/142292se.pdf
# Consta 2x1 no site, a súmula está certa mas não tem os gols no site

# Problemas entre 2014 e 2019 SÉRIE B:

# Jogo 170 de 2019
# https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-b/2019/170#documentos
# https://conteudo.cbf.com.br/sumulas/2019/242170se.pdf
# Site certo mas sem gols, súmula diz 3x0 mas não diz o tempo dos gols
# https://globoesporte.globo.com/mt/futebol/brasileirao-serie-b/jogo/20-08-2019/cuiaba-figueirense.ghtml
# Foi WO, colocar como NA.

# Jogo 7 de 2014
# https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-b/2014/7
# https://conteudo.cbf.com.br/sumulas/2014/2427s.pdf
# 3x0 sem gols no site, súmula em scan (WTF?)
# http://globoesporte.globo.com/futebol/brasileirao-serie-b/noticia/2014/04/portuguesa-deixa-gramado-jogo-em-joinville-para-e-brasileiro-fica-sob-risco.html
# Foi WO, colocar como NA.

