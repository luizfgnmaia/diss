
library(dplyr)
library(pdftools)
library(stringr)

first_half = NULL
second_half = NULL
i = 0

for(ano in 2015:2020) {
  for(jogo in 1:380) {
    i = i + 1
    url = paste0("https://conteudo.cbf.com.br/sumulas/", ano, "/142", jogo, "se.pdf")
    
    extract = url %>% 
      pdf_text() %>%
      str_squish() %>%
      .[1] %>%
      str_extract_all("(?<=Acréscimo:\\s).{1,3}(?=\\s)") %>%
      unlist()
    
    first_half[i] = extract[1]
    second_half[i] = extract[2]
    
    print(paste0("Ano = ", ano, ", Jogo = ", jogo))
  }
}

first_half[which(first_half == "Não")] = "0"
second_half[which(second_half == "Não")] = "0"
first_half = as.integer(first_half)
second_half = as.integer(second_half)

rm(list = setdiff(ls(), c("first_half", "second_half")))
save.image("scrape/data/scrape_st.RData")
