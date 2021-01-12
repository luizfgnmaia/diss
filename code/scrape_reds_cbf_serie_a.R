
library(dplyr)
library(tabulizer)
library(stringr)

lst_red = list()
lst_ano = list()
lst_jogo = list()
lst_confronto = list()
i = 0

for(ano in 2014:2019) {
  for(jogo in 1:380) {
    i = i + 1
    url = paste0("https://conteudo.cbf.com.br/sumulas/", ano, "/142", jogo, "se.pdf")
    
    tab = extract_tables(url, encoding = "UTF-8", guess = FALSE)
    
    for(j in 1:length(tab)) {
      tab[[j]] = as_tibble(tab[[j]]) %>%
        tidyr::unite(col = "V1", sep = " ")
    }
    tab = do.call(rbind, tab) %>%
      mutate(V1 = str_squish(V1))
    
    confronto = str_extract(tab$V1, "(?<=Jogo:\\s)[A-Z].*") %>%
      na.omit() %>%
      .[1]
    
    cartoes_vermelhos = which(tab$V1 == "Cartões Vermelhos")
    ocorrencias = which(tab$V1 == "Ocorrências / Observações")
    relatorio = which(tab$V1 == "Relatório do Assistente") # ano = 2014; jogo = 284 não tem ocorrências
    if(length(ocorrencias) == 0) {
     ocorrencias = relatorio 
    }
    
    tab = tab[cartoes_vermelhos:ocorrencias,] %>%
      filter(V1 != "2o Cartão Amarelo")
    
    primeiro_caractere = substr(tab$V1, start = 1, stop = 1)
    terceiro_ultimo_caractere = substr(tab$V1, start = nchar(tab$V1)-2, stop = nchar(tab$V1)-2)
    
    tab = tab[which(primeiro_caractere %in% c("+", "-", "P", 0:9) & terceiro_ultimo_caractere == "/"),] 
    
    tab = tab %>% # para tirar o 2 / 3 do rodapé
      mutate(nchar = nchar(V1)) %>%
      filter(nchar > 5) %>%
      select(-nchar) 
    
    len = nrow(tab)
    
    if(len > 0) {
      lst_red[[i]] = tab$V1
      lst_ano[[i]] = rep(ano, len)
      lst_jogo[[i]] = rep(jogo, len)
      lst_confronto[[i]] = rep(confronto, len)
    }
    print(paste0("Ano = ", ano, ", Jogo = ", jogo))
  }
}

save.image("data/scrape_reds_cbf_serie_a.RData")

