
# https://basedosdados.org/dataset/mundo-transfermarkt-competicoes/resource/23e8dde9-ffb8-4e7c-a92b-83d4ef9d2c79

library("basedosdados")

set_billing_id("inductive-seat-313801") # colocar o id aqui

query = "SELECT * FROM `basedosdados.mundo_transfermarkt_competicoes.brasileirao_serie_a`"

transfermarkt = read_sql(query) %>%
  mutate(ano_campeonato = as.integer(ano_campeonato),
         rodada = as.integer(rodada),
         publico = as.integer(publico),
         publico_max = as.integer(publico_max),
         colocacao_man = as.integer(colocacao_man),
         colocacao_vis = as.integer(colocacao_vis),
         valor_equipe_titular_man = as.integer(valor_equipe_titular_man),
         valor_equipe_titular_vis = as.integer(valor_equipe_titular_vis),
         gols_man = as.integer(gols_man),
         gols_vis = as.integer(gols_vis),
         gols_1_tempo_man = as.integer(gols_1_tempo_man),
         gols_1_tempo_vis = as.integer(gols_1_tempo_vis),
         escanteios_man = as.integer(escanteios_man),
         escanteios_vis = as.integer(escanteios_vis),
         faltas_man = as.integer(faltas_man),
         faltas_vis = as.integer(faltas_vis),
         chutes_bola_parada_man = as.integer(chutes_bola_parada_man),
         chutes_bola_parada_vis = as.integer(chutes_bola_parada_vis),
         defesas_man = as.integer(defesas_man),
         defesas_vis = as.integer(defesas_vis),
         impedimentos_man = as.integer(impedimentos_man),
         impedimentos_vis = as.integer(impedimentos_vis),
         chutes_man = as.integer(chutes_man),
         chutes_vis = as.integer(chutes_vis),
         chutes_fora_man = as.integer(chutes_fora_man),
         chutes_fora_vis = as.integer(chutes_fora_vis))

save(transfermarkt, file = "transfermarkt/data/transfermarkt.RData")
