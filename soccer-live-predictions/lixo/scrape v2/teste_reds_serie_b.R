
library(dplyr)
library(rvest)

load("data/scrape_reds_cbf_serie_b.RData")

# 2019 111 vs 113
length(unlist(lst_red[1901:2280]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-b/2019") %>%
  html_nodes(".expand-trigger .hidden-xs:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

# 2018 OK
length(unlist(lst_red[1521:1900]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-b/2018") %>%
  html_nodes(".expand-trigger .hidden-xs:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

# 2017 OK
length(unlist(lst_red[1141:1520]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-b/2017") %>%
  html_nodes("td:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

# 2016 OK
length(unlist(lst_red[761:1140]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-b/2016") %>%
  html_nodes("td:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

# 2015 OK
length(unlist(lst_red[381:760]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-b/2015") %>%
  html_nodes("td:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

# 2014 OK
length(unlist(lst_red[1:380]))
read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-b/2014") %>%
  html_nodes("td:nth-child(11)") %>%
  html_text() %>%
  as.numeric() %>%
  sum()

#################################################################################

manual = readxl::read_excel("reds_manual.xlsx", sheet = 2)

sum(manual$Reds)

scrape = lapply(lst_red, length) %>%
  unlist()

which(manual$Reds - scrape[1901:2280] != 0)
