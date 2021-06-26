
pnk_Result <- function(Score, Home, Draw, Away) {
  if(Score == 1) {
    return(Home)
  } else if(Score == 2) {
    return(Draw)
  } else {
    return(Away)
  }
}

load("odds/data/serie_a_open_means.RData")
load("weight/data/HDA_dc_2.RData")

HDA_open = left_join(HDA_dc, serie_a_open_means, by = c("Season", "Home_Team", "Away_Team")) %>%
  rowwise() %>%
  mutate(pnk_Result_open = pnk_Result(Result, Home_open, Draw_open, Away_open))

save(HDA_open, file = "odds/data/HDA_open.RData")

