
library(stringr)

fix_date <- function(data) {
  
  ret = NULL
  
  for(i in 1:length(data)) {
    ddmmyyyy = str_extract(data[i], "(?<=,\\s).*") %>%
      str_replace_all("de ", "") %>%
      str_replace_all("Janeiro", "01") %>%
      str_replace_all("Fevereiro", "02") %>%
      str_replace_all("Março", "03") %>%
      str_replace_all("Abril", "04") %>%
      str_replace_all("Maio", "05") %>%
      str_replace_all("Junho", "06") %>%
      str_replace_all("Julho", "07") %>%
      str_replace_all("Agosto", "08") %>%
      str_replace_all("Setembro", "09") %>%
      str_replace_all("Outubro", "10") %>%
      str_replace_all("Novembro", "11") %>%
      str_replace_all("Dezembro", "12") %>%
      str_split(" ") %>%
      unlist()
    
    ret[i] = paste(ddmmyyyy[3], ddmmyyyy[2], ddmmyyyy[1], sep = "-")
  }
  
  ret
}  
  
