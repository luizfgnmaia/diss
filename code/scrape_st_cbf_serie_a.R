
library(dplyr)
library(pdftools)
library(stringr)

first_half_serie_a = NULL
second_half_serie_a = NULL
i = 0

for(ano in 2014:2019) {
  for(jogo in 1:380) {
    i = i + 1
    url = paste0("https://conteudo.cbf.com.br/sumulas/", ano, "/142", jogo, "se.pdf")
    
    extract = url %>% 
      pdf_text() %>%
      str_squish() %>%
      .[1] %>%
      str_extract_all("(?<=Acréscimo:\\s).{1,3}(?=\\s)") %>%
      unlist()
    
    first_half_serie_a[i] = extract[1]
    second_half_serie_a[i] = extract[2]
    
    print(paste0("Ano = ", ano, ", Jogo = ", jogo))
  }
}

first_half_serie_a[which(first_half_serie_a == "Não")] = "0"
second_half_serie_a[which(second_half_serie_a == "Não")] = "0"
first_half_serie_a = as.integer(first_half_serie_a)
second_half_serie_a = as.integer(second_half_serie_a)

rm(list = setdiff(ls(), c("first_half_serie_a", "second_half_serie_a")))
save.image("data/scrape_st_cbf_serie_a.RData")
