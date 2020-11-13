
library(rvest)

try_again <- function(expr, max_tries = Inf, sleep = 0) {
  out = try(eval(expr))
  n = 1
  while(class(out) == "try-error" & n < max_tries) {
    out = try(eval(expr))
    n = n + 1
    Sys.sleep(sleep)
  }
  out
}

get_text <- function(pag, node) {
  ret = pag %>% 
    html_nodes(node) %>%
    html_text(trim = TRUE)
  if(length(ret > 0)) {
    return(ret)
  } else {
    return(NA)
  }
}

time_1 = NULL
time_2 = NULL
placar_1 = NULL
placar_2 = NULL
gols_1 = list()
gols_2 = list()
data = NULL
i = 0

for(ano in 2013:2019) {
  for(jogo in 1:380) {
    i = i + 1
    url = paste0("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/",
                 ano, "/", jogo)
    pag = try_again(read_html(url))
    time_1[i] = get_text(pag, ".time-left .color-white")
    time_2[i] = get_text(pag, ".time-right .color-white")
    placar_1[i] = get_text(pag, ".text-left .time-gols")
    placar_2[i] = get_text(pag, ".text-right .time-gols")
    gols_1[[i]] = get_text(pag, ".text-left .time-jogador")
    gols_2[[i]] = get_text(pag, ".text-right .time-jogador")
    data[i] = get_text(pag, "#menu-panel > article > section.section-placar.p-b-100 > div > div > div > header > div > div.col-sm-8 > span:nth-child(2)")
    print(paste0("Ano = ", ano, ", Jogo = ", jogo))
  }
}

# https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2019/371 Não tem o placar do jogo no site
dir.create("data")
save.image("data/scrape_cbf_serie_a.RData")

