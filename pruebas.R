datos <- read.table("Datos_bn.txt")

datos_votos <- datos %>% 
  group_by(Provincia, Municipio) %>% mutate(PorcMuni = VotosMuni/sum(VotosMuni)*100) %>% 
  group_by(Provincia, Partido) %>% mutate(Votos = sum(VotosMuni)) %>% 
  group_by(Partido) %>% mutate(VotosCCAA = sum(VotosMuni))

votos <- datos_votos %>% group_by(Partido) %>% 
  select(Partido, VotosCCAA) %>% distinct() %>% ungroup() %>%
  rename(Votos = VotosCCAA)

bn <- votos %>% filter(Partido == "Votos en blanco" | Partido == "Votos nulos")
votos <- votos %>% filter(Partido != "Votos en blanco" & Partido != "Votos nulos")

votos <- votos %>% bind_rows(bn)
