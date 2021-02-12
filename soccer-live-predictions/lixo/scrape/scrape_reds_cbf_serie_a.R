
library(dplyr)
library(pdftools)
library(stringr)

min = list()
tempo = list()
time = list()
i = 0

for(ano in 2014:2019) {
  for(jogo in 1:380) {
    i = i + 1
    url = paste0("https://conteudo.cbf.com.br/sumulas/", ano, "/142", jogo, "se.pdf")

    text = pdf_text(url) %>%
      str_squish()
    
    loc = text[[2]] %>%
      str_locate("Cartões Vermelhos Tempo 1T/2T Nº Nome do Jogador ") 
    
    sub = text[[2]] %>%
      substr(start = loc[2]+1, stop = 9999)
    
    min[[i]] = str_extract_all(sub, "[+0-9]*(?=\\:00\\s[1-2]T\\s)") %>%
      unlist() %>%
      na.omit() %>%
      as.vector() %>%
      .[. != ""]
    
    tempo[[i]] = str_extract_all(sub, "[12](?=T)") %>%
      unlist()
    
    time[[i]] = str_extract_all(sub, "(?<=\\s\\-\\s)(.{1,70}\\/[A-Z]{1,2})(?=\\s)") %>%
      unlist()
    
    print(paste0("Ano = ", ano, ", Jogo = ", jogo))
  }
}

# Conserto manual
time[[211]] = time[[211]][-1] # https://conteudo.cbf.com.br/sumulas/2014/142211se.pdf

save.image("data/scrape_reds_cbf_serie_a.RData")

