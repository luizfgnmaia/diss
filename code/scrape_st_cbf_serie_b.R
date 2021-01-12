
library(dplyr)
library(pdftools)
library(stringr)

first_half_serie_b = NULL
second_half_serie_b = NULL
i = 0

for(ano in 2014:2019) {
  for(jogo in 1:380) {
    i = i + 1
    url = paste0("https://conteudo.cbf.com.br/sumulas/", ano, "/242", jogo, "se.pdf")
    
    if(url != "https://conteudo.cbf.com.br/sumulas/2014/2427se.pdf") { # sétima partida de 2014
      
      extract = url %>% 
        pdf_text() %>%
        str_squish() %>%
        .[1] %>%
        str_extract_all("(?<=Acréscimo:\\s).{1,3}(?=\\s)") %>%
        unlist()
      
      first_half_serie_b[i] = extract[1]
      second_half_serie_b[i] = extract[2]
      
      print(paste0("Ano = ", ano, ", Jogo = ", jogo))
    }
  }
}

first_half_serie_b[which(first_half_serie_b == "Não")] = "0"
second_half_serie_b[which(second_half_serie_b == "Não")] = "0"
first_half_serie_b = as.integer(first_half_serie_b)
second_half_serie_b = as.integer(second_half_serie_b)

rm(list = setdiff(ls(), c("first_half_serie_b", "second_half_serie_b")))
save.image("data/scrape_st_cbf_serie_b.RData")
