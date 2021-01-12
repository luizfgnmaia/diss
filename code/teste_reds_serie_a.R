
library(dplyr)
library(rvest)

load("data/scrape_reds_cbf_serie_a.RData")

# 2019 OK
length(unlist(lst_red[1901:2280]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2019") %>%
  html_nodes(".expand-trigger .hidden-xs:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

# 2018 OK
length(unlist(lst_red[1521:1900]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2018") %>%
  html_nodes(".expand-trigger .hidden-xs:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

# 2017 FALTA 1
length(unlist(lst_red[1141:1520]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2017") %>%
  html_nodes("td:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

# 2016 OK
length(unlist(lst_red[761:1140]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2016") %>%
  html_nodes("td:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

# 2015 OK
length(unlist(lst_red[381:760]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2015") %>%
  html_nodes("td:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

# 2014 OK
length(unlist(lst_red[1:380]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2014") %>%
  html_nodes("td:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()


